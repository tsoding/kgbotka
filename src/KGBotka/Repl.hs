{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Repl
  ( replThread
  , backdoorThread
  , ReplState(..)
  , ReplCommand(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import Irc.Identifier (Identifier, idText, mkId)
import KGBotka.Command
import KGBotka.Config
import KGBotka.Queue
import KGBotka.Roles
import KGBotka.Sqlite
import KGBotka.TwitchAPI
import qualified Network.HTTP.Client as HTTP
import System.IO
import Network.Socket
import Control.Exception

data ReplState = ReplState
  { replStateChannels :: !(TVar (S.Set Identifier))
  , replStateSqliteFileName :: !FilePath
  , replStateCurrentChannel :: !(Maybe T.Text)
  , replStateCommandQueue :: !(WriteQueue ReplCommand)
  , replStateConfigTwitch :: !ConfigTwitch
  , replStateManager :: !HTTP.Manager
  , replStateHandle :: !Handle
  }

data ReplCommand
  = Say T.Text
        T.Text
  | JoinChannel T.Text
  | PartChannel Identifier

replThread :: ReplState -> IO ()
replThread initState
  -- TODO: redesign Bot and Repl threads with database lock situations in mind
 =
  withConnectionAndPragmas (replStateSqliteFileName initState) $ \conn -> do
    Sqlite.execute_ conn "PRAGMA foreign_keys=ON"
    replThread' conn initState

replThread' :: Sqlite.Connection -> ReplState -> IO ()
replThread' dbConn state = do
  let replHandle = replStateHandle state
  hPutStr replHandle $
    "[" <> T.unpack (fromMaybe "#" $ replStateCurrentChannel state) <> "]> "
  hFlush (replStateHandle state)
  cmd <- T.words . T.pack <$> getLine
  case (cmd, replStateCurrentChannel state) of
    ("cd":channel:_, _) ->
      replThread' dbConn $ state {replStateCurrentChannel = Just channel}
    ("cd":_, _) ->
      replThread' dbConn $ state {replStateCurrentChannel = Nothing}
    ("say":args, Just channel) -> do
      atomically $
        writeQueue (replStateCommandQueue state) $ Say channel $ T.unwords args
      replThread' dbConn state
    ("say":_, Nothing) -> do
      hPutStrLn replHandle "No current channel to say anything to is selected"
      replThread' dbConn state
    ("quit":_, _) -> return ()
    ("q":_, _) -> return ()
    ("join":channel:_, _) -> do
      atomically $
        writeQueue (replStateCommandQueue state) $ JoinChannel channel
      replThread' dbConn $ state {replStateCurrentChannel = Just channel}
    ("part":_, Just channel) -> do
      atomically $ do
        let channelId = mkId channel
        isMember <- S.member channelId <$> readTVar (replStateChannels state)
        when isMember $
          writeQueue (replStateCommandQueue state) $ PartChannel channelId
      replThread' dbConn $ state {replStateCurrentChannel = Nothing}
    ("ls":_, _) -> do
      traverse_ (hPutStrLn replHandle . T.unpack . idText) =<<
        S.toList <$> readTVarIO (replStateChannels state)
      replThread' dbConn state
    ("addcmd":name:args, _) -> do
      Sqlite.withTransaction dbConn $ addCommand dbConn name (T.unwords args)
      replThread' dbConn state
    ("addalias":alias:name:_, _) -> do
      Sqlite.withTransaction dbConn $ addCommandName dbConn alias name
      replThread' dbConn state
    ("delcmd":name:_, _) -> do
      Sqlite.withTransaction dbConn $ deleteCommandByName dbConn name
      replThread' dbConn state
    ("delalias":name:_, _) -> do
      Sqlite.withTransaction dbConn $ deleteCommandName dbConn name
      replThread' dbConn state
    ("assrole":roleName:users, _) -> do
      Sqlite.withTransaction dbConn $ do
        maybeRole <- getTwitchRoleByName dbConn roleName
        response <-
          HTTP.responseBody . unwrapJsonResponse <$>
          getUsersByLogins
            (replStateManager state)
            (configTwitchClientId $ replStateConfigTwitch state)
            users
        case (response, maybeRole) of
          (Right twitchUsers, Just role') ->
            traverse_
              (assTwitchRoleToUser dbConn (twitchRoleId role') . twitchUserId)
              twitchUsers
          (Left message, _) -> hPutStrLn replHandle $ "[ERROR] " <> message
          (_, Nothing) -> hPutStrLn replHandle "[ERROR] Such role does not exist"
      replThread' dbConn state
    (unknown:_, _) -> do
      hPutStrLn replHandle $ T.unpack $ "Unknown command: " <> unknown
      replThread' dbConn state
    _ -> replThread' dbConn state

-- TODO: backdoor does not log incoming connections
backdoorThread :: String -> WriteQueue ReplCommand -> IO ()
backdoorThread port' _{-replCommands-} = do
  addr <- resolve port'
  bracket (open addr) close loop
  where
    resolve port = do
      let hints =
            defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
      addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      setCloseOnExecIfNeeded $ fdSocket sock
      listen sock 10
      return sock
    loop sock = do
      (conn, _) <- accept sock
      void $ forkFinally (talk conn) (const $ close conn)
      loop sock
    talk _{-conn-} = undefined --do
      --connHandle <- socketToHandle conn ReadWriteMode
      --evalStateT (consoleEnv console) $ HandleChannel connHandle connHandle

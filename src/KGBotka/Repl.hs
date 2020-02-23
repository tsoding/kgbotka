{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Repl
  ( replThread
  , backdoorThread
  , backdoorLoggingThread
  , ReplState(..)
  , ReplCommand(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Except
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import KGBotka.Bttv
import KGBotka.Command
import KGBotka.Config
import KGBotka.Ffz
import KGBotka.Queue
import KGBotka.Roles
import KGBotka.Sqlite
import KGBotka.TwitchAPI
import qualified Network.HTTP.Client as HTTP
import Network.Socket
import System.IO
import KGBotka.Log

data ReplState = ReplState
  { replStateChannels :: !(TVar (S.Set TwitchIrcChannel))
  , replStateSqliteFileName :: !FilePath
  , replStateCurrentChannel :: !(Maybe TwitchIrcChannel)
  , replStateCommandQueue :: !(WriteQueue ReplCommand)
  , replStateConfigTwitch :: !ConfigTwitch
  , replStateManager :: !HTTP.Manager
  , replStateHandle :: !Handle
  , replStateLogQueue :: WriteQueue T.Text
  , replStateConnAddr :: Maybe SockAddr
  }

data ReplCommand
  = Say TwitchIrcChannel
        T.Text
  | JoinChannel TwitchIrcChannel
  | PartChannel TwitchIrcChannel

replThread :: ReplState -> IO ()
replThread initState =
  withConnectionAndPragmas (replStateSqliteFileName initState) $ \conn -> do
    Sqlite.execute_ conn "PRAGMA foreign_keys=ON"
    replThread' conn initState

-- TODO(#60): there is no shutdown command that shuts down the whole bot
-- Since we introduce backdoor connections the quit command does
-- not serve such purpose anymore, 'cause it only closes the current
-- REPL connection
-- TODO(#65): there is no `who` command that would show all of the Backdoor connections
replThread' :: Sqlite.Connection -> ReplState -> IO ()
replThread' dbConn state = do
  let replHandle = replStateHandle state
  let withTransactionLogErrors :: IO () -> IO ()
      withTransactionLogErrors f =
        catch
          (Sqlite.withTransaction dbConn f)
          (\e -> hPrint replHandle (e :: Sqlite.SQLError))
  hPutStr replHandle $
    "[" <>
    T.unpack
      (twitchIrcChannelText $ fromMaybe "#" $ replStateCurrentChannel state) <>
    "]> "
  hFlush (replStateHandle state)
  inputLine <- T.pack <$> hGetLine replHandle
  atomically $
    writeQueue (replStateLogQueue state) $
    T.pack (show $ replStateConnAddr state) <> ": " <> inputLine
  case (T.words inputLine, replStateCurrentChannel state) of
    ("cd":channel:_, _) ->
      replThread' dbConn $
      state {replStateCurrentChannel = Just $ mkTwitchIrcChannel channel}
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
        writeQueue (replStateCommandQueue state) $
        JoinChannel $ mkTwitchIrcChannel channel
      replThread' dbConn $
        state {replStateCurrentChannel = Just $ mkTwitchIrcChannel channel}
    ("part":_, Just channel) -> do
      atomically $ do
        isMember <- S.member channel <$> readTVar (replStateChannels state)
        when isMember $
          writeQueue (replStateCommandQueue state) $ PartChannel channel
      replThread' dbConn $ state {replStateCurrentChannel = Nothing}
    ("ls":_, _) -> do
      traverse_ (hPutStrLn replHandle . T.unpack . twitchIrcChannelText) =<<
        S.toList <$> readTVarIO (replStateChannels state)
      replThread' dbConn state
    ("addcmd":name:args, _) -> do
      withTransactionLogErrors $ addCommand dbConn name (T.unwords args)
      replThread' dbConn state
    ("addalias":alias:name:_, _) -> do
      withTransactionLogErrors $ addCommandName dbConn alias name
      replThread' dbConn state
    ("updatebttv":_, channel) -> do
      withTransactionLogErrors $ do
        result <-
          runExceptT $ updateBttvEmotes dbConn (replStateManager state) channel
        case (result, channel) of
          (Right (), Nothing) ->
            hPutStrLn replHandle "Global BTTV emotes are updated"
          (Right (), Just channelName) ->
            hPutStrLn replHandle $
            "BTTV emotes are updated for channel " <>
            T.unpack (twitchIrcChannelText channelName)
          (Left message, _) -> hPutStrLn replHandle $ "[ERROR] " <> message
      replThread' dbConn state
    ("updateffz":_, channel) -> do
      withTransactionLogErrors $ do
        result <-
          runExceptT $ updateFfzEmotes dbConn (replStateManager state) channel
        case (result, channel) of
          (Right (), Nothing) ->
            hPutStrLn replHandle "Global FFZ emotes are updated"
          (Right (), Just channelName) ->
            hPutStrLn replHandle $
            "FFZ emotes are updated for channel " <>
            T.unpack (twitchIrcChannelText channelName)
          (Left message, _) -> hPutStrLn replHandle $ "[ERROR] " <> message
      replThread' dbConn state
    ("addrole":name:_, _) -> do
      withTransactionLogErrors $ do
        role <- getTwitchRoleByName dbConn name
        case role of
          Just _ ->
            hPutStrLn replHandle $ "Role " <> T.unpack name <> " already exists"
          Nothing -> do
            addTwitchRole dbConn name
            hPutStrLn replHandle $ "Added a new role: " <> T.unpack name
      replThread' dbConn state
    ("lsroles":_, _) -> do
      withTransactionLogErrors $ do
        roles <- listTwitchRoles dbConn
        mapM_ (hPutStrLn replHandle . T.unpack . twitchRoleName) roles
      replThread' dbConn state
    ("delcmd":name:_, _) -> do
      withTransactionLogErrors $ deleteCommandByName dbConn name
      replThread' dbConn state
    ("delalias":name:_, _) -> do
      withTransactionLogErrors $ deleteCommandName dbConn name
      replThread' dbConn state
    ("assrole":roleName:users, _) -> do
      withTransactionLogErrors $ do
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
          (_, Nothing) ->
            hPutStrLn replHandle "[ERROR] Such role does not exist"
      replThread' dbConn state
    (unknown:_, _) -> do
      hPutStrLn replHandle $ T.unpack $ "Unknown command: " <> unknown
      replThread' dbConn state
    _ -> replThread' dbConn state

backdoorThread :: String -> ReplState -> IO ()
backdoorThread port initState = do
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
  bracket (open addr) close loop
  where
    hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      setCloseOnExecIfNeeded $ fdSocket sock
      listen sock 10
      return sock
    loop sock = do
      (conn, addr) <- accept sock
      -- TODO(#62): backdoor repl connection is not always closed upon the quit command
      void $ forkFinally (talk conn addr) (const $ close conn)
      loop sock
    talk conn addr = do
      atomically $
        writeQueue
          (replStateLogQueue initState)
          (T.pack (show addr) <> " has connected to the Backdoor gachiBASS")
      connHandle <- socketToHandle conn ReadWriteMode
      replThread $
        initState {replStateHandle = connHandle, replStateConnAddr = Just addr}

-- TODO(#82): there is no REPL mechanism to update command cooldown

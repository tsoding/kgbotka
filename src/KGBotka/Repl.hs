{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Repl
  ( replThread
  , backdoorThread
  , ReplThreadParams(..)
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
import KGBotka.Ffz
import KGBotka.Log
import KGBotka.Queue
import KGBotka.Roles
import KGBotka.Sqlite
import KGBotka.TwitchAPI
import qualified Network.HTTP.Client as HTTP
import Network.Socket
import System.IO

data ReplThreadParams = ReplThreadParams
  { rtpChannels :: !(TVar (S.Set TwitchIrcChannel))
  , rtpSqliteFileName :: !FilePath
  , rtpCurrentChannel :: !(Maybe TwitchIrcChannel)
  , rtpCommandQueue :: !(WriteQueue ReplCommand)
  , rtpTwitchClientId :: Maybe T.Text
  , rtpManager :: !HTTP.Manager
  , rtpHandle :: !Handle
  , rtpLogQueue :: !(WriteQueue LogEntry)
  , rtpConnAddr :: !(Maybe SockAddr)
  }

data ReplCommand
  = Say TwitchIrcChannel
        T.Text
  | JoinChannel TwitchIrcChannel
  | PartChannel TwitchIrcChannel

replThread :: ReplThreadParams -> IO ()
replThread initState =
  withConnectionAndPragmas (rtpSqliteFileName initState) $ \conn -> do
    Sqlite.execute_ conn "PRAGMA foreign_keys=ON"
    replThread' conn initState

-- TODO(#60): there is no shutdown command that shuts down the whole bot
-- Since we introduce backdoor connections the quit command does
-- not serve such purpose anymore, 'cause it only closes the current
-- REPL connection
-- TODO(#65): there is no `who` command that would show all of the Backdoor connections
replThread' :: Sqlite.Connection -> ReplThreadParams -> IO ()
replThread' dbConn state = do
  let replHandle = rtpHandle state
  let withTransactionLogErrors :: IO () -> IO ()
      withTransactionLogErrors f =
        catch
          (Sqlite.withTransaction dbConn f)
          (\e -> hPrint replHandle (e :: Sqlite.SQLError))
  hPutStr replHandle $
    "[" <>
    T.unpack
      (twitchIrcChannelText $ fromMaybe "#" $ rtpCurrentChannel state) <>
    "]> "
  hFlush (rtpHandle state)
  inputLine <- T.pack <$> hGetLine replHandle
  atomically $
    writeQueue (rtpLogQueue state) $
    LogEntry "BACKDOOR" $
    T.pack (show $ rtpConnAddr state) <> ": " <> inputLine
  case (T.words inputLine, rtpCurrentChannel state) of
    ("cd":channel:_, _) ->
      replThread' dbConn $
      state {rtpCurrentChannel = Just $ mkTwitchIrcChannel channel}
    ("cd":_, _) ->
      replThread' dbConn $ state {rtpCurrentChannel = Nothing}
    ("say":args, Just channel) -> do
      atomically $
        writeQueue (rtpCommandQueue state) $ Say channel $ T.unwords args
      replThread' dbConn state
    ("say":_, Nothing) -> do
      hPutStrLn replHandle "No current channel to say anything to is selected"
      replThread' dbConn state
    ("quit":_, _) -> return ()
    ("q":_, _) -> return ()
    ("join":channel:_, _) -> do
      atomically $
        writeQueue (rtpCommandQueue state) $
        JoinChannel $ mkTwitchIrcChannel channel
      replThread' dbConn $
        state {rtpCurrentChannel = Just $ mkTwitchIrcChannel channel}
    ("part":_, Just channel) -> do
      atomically $ do
        isMember <- S.member channel <$> readTVar (rtpChannels state)
        when isMember $
          writeQueue (rtpCommandQueue state) $ PartChannel channel
      replThread' dbConn $ state {rtpCurrentChannel = Nothing}
    ("ls":_, _) -> do
      traverse_ (hPutStrLn replHandle . T.unpack . twitchIrcChannelText) =<<
        S.toList <$> readTVarIO (rtpChannels state)
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
          runExceptT $ updateBttvEmotes dbConn (rtpManager state) channel
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
          runExceptT $ updateFfzEmotes dbConn (rtpManager state) channel
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
      withTransactionLogErrors $
        case rtpTwitchClientId state of
          Just clientId -> do
            maybeRole <- getTwitchRoleByName dbConn roleName
            response <-
              HTTP.responseBody <$>
              getUsersByLogins (rtpManager state) clientId users
            case (response, maybeRole) of
              (Right twitchUsers, Just role') ->
                traverse_
                  (assTwitchRoleToUser dbConn (twitchRoleId role') .
                   twitchUserId)
                  twitchUsers
              (Left message, _) -> hPutStrLn replHandle $ "[ERROR] " <> message
              (_, Nothing) ->
                hPutStrLn replHandle "[ERROR] Such role does not exist"
          Nothing -> hPutStrLn replHandle "[ERROR] No twitch configuration"
      replThread' dbConn state
    (unknown:_, _) -> do
      hPutStrLn replHandle $ T.unpack $ "Unknown command: " <> unknown
      replThread' dbConn state
    _ -> replThread' dbConn state

backdoorThread :: String -> ReplThreadParams -> IO ()
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
        writeQueue (rtpLogQueue initState) $
        LogEntry "BACKDOOR" $
        T.pack (show addr) <> " has connected to the Backdoor gachiBASS"
      connHandle <- socketToHandle conn ReadWriteMode
      replThread $ initState {rtpHandle = connHandle, rtpConnAddr = Just addr}
-- TODO(#82): there is no REPL mechanism to update command cooldown

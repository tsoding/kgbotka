{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Repl
  ( backdoorThread
  , ReplCommand(..)
  , BackdoorThreadParams(..)
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
import KGBotka.Http
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
  , rtpCommandQueue :: !(WriteQueue ReplCommand)
  , rtpTwitchClientId :: Maybe T.Text
  , rtpManager :: !HTTP.Manager
  , rtpHandle :: !Handle
  , rtpLogQueue :: !(WriteQueue LogEntry)
  , rtpConnAddr :: !SockAddr
  }

instance ProvidesLogging ReplThreadParams where
  logQueue = rtpLogQueue

data ReplThreadState = ReplThreadState
  { rtsChannels :: !(TVar (S.Set TwitchIrcChannel))
  , rtsSqliteConnection :: !Sqlite.Connection
  , rtsCurrentChannel :: !(Maybe TwitchIrcChannel)
  , rtsCommandQueue :: !(WriteQueue ReplCommand)
  , rtsTwitchClientId :: Maybe T.Text
  , rtsManager :: !HTTP.Manager
  , rtsHandle :: !Handle
  , rtsLogQueue :: !(WriteQueue LogEntry)
  , rtsConnAddr :: !SockAddr
  }

instance ProvidesDatabase ReplThreadState where
  getSqliteConnection = rtsSqliteConnection

instance ProvidesHttpManager ReplThreadState where
  httpManager = rtsManager

data ReplCommand
  = Say TwitchIrcChannel
        T.Text
  | JoinChannel TwitchIrcChannel
  | PartChannel TwitchIrcChannel

replThread :: ReplThreadParams -> IO ()
replThread rtp =
  withConnectionAndPragmas (rtpSqliteFileName rtp) $ \conn -> do
    Sqlite.execute_ conn "PRAGMA foreign_keys=ON"
    replThreadLoop
      ReplThreadState
        { rtsChannels = rtpChannels rtp
        , rtsSqliteConnection = conn
        , rtsCurrentChannel = Nothing
        , rtsCommandQueue = rtpCommandQueue rtp
        , rtsTwitchClientId = rtpTwitchClientId rtp
        , rtsManager = rtpManager rtp
        , rtsHandle = rtpHandle rtp
        , rtsLogQueue = rtpLogQueue rtp
        , rtsConnAddr = rtpConnAddr rtp
        }

-- TODO(#60): there is no shutdown command that shuts down the whole bot
-- Since we introduce backdoor connections the quit command does
-- not serve such purpose anymore, 'cause it only closes the current
-- REPL connection
-- TODO(#65): there is no `who` command that would show all of the Backdoor connections
replThreadLoop :: ReplThreadState -> IO ()
replThreadLoop rts = do
  let replHandle = rtsHandle rts
  let withTransactionLogErrors :: IO () -> IO ()
      withTransactionLogErrors f =
        catch
          (Sqlite.withTransaction (rtsSqliteConnection rts) f)
          (\e -> hPrint replHandle (e :: Sqlite.SQLError))
  hPutStr replHandle $
    "[" <>
    T.unpack (twitchIrcChannelText $ fromMaybe "#" $ rtsCurrentChannel rts) <>
    "]> "
  hFlush (rtsHandle rts)
  inputLine <- T.pack <$> hGetLine replHandle
  atomically $
    writeQueue (rtsLogQueue rts) $
    LogEntry "BACKDOOR" $ T.pack (show $ rtsConnAddr rts) <> ": " <> inputLine
  case (T.words inputLine, rtsCurrentChannel rts) of
    ("cd":channel:_, _) ->
      replThreadLoop $
      rts {rtsCurrentChannel = Just $ mkTwitchIrcChannel channel}
    ("cd":_, _) -> replThreadLoop $ rts {rtsCurrentChannel = Nothing}
    ("say":args, Just channel) -> do
      atomically $
        writeQueue (rtsCommandQueue rts) $ Say channel $ T.unwords args
      replThreadLoop rts
    ("say":_, Nothing) -> do
      hPutStrLn replHandle "No current channel to say anything to is selected"
      replThreadLoop rts
    ("quit":_, _) -> return ()
    ("q":_, _) -> return ()
    ("join":channel:_, _) -> do
      atomically $
        writeQueue (rtsCommandQueue rts) $
        JoinChannel $ mkTwitchIrcChannel channel
      replThreadLoop $
        rts {rtsCurrentChannel = Just $ mkTwitchIrcChannel channel}
    ("part":_, Just channel) -> do
      atomically $ do
        isMember <- S.member channel <$> readTVar (rtsChannels rts)
        when isMember $ writeQueue (rtsCommandQueue rts) $ PartChannel channel
      replThreadLoop $ rts {rtsCurrentChannel = Nothing}
    ("ls":_, _) -> do
      traverse_ (hPutStrLn replHandle . T.unpack . twitchIrcChannelText) =<<
        S.toList <$> readTVarIO (rtsChannels rts)
      replThreadLoop rts
    ("addcmd":name:args, _) -> do
      withTransactionLogErrors $ addCommand rts name (T.unwords args)
      replThreadLoop rts
    ("addalias":alias:name:_, _) -> do
      withTransactionLogErrors $ addCommandName rts alias name
      replThreadLoop rts
    ("updatebttv":_, channel) -> do
      withTransactionLogErrors $ do
        result <- runExceptT $ updateBttvEmotes rts channel
        case (result, channel) of
          (Right (), Nothing) ->
            hPutStrLn replHandle "Global BTTV emotes are updated"
          (Right (), Just channelName) ->
            hPutStrLn replHandle $
            "BTTV emotes are updated for channel " <>
            T.unpack (twitchIrcChannelText channelName)
          (Left message, _) -> hPutStrLn replHandle $ "[ERROR] " <> message
      replThreadLoop rts
    ("updateffz":_, channel) -> do
      withTransactionLogErrors $ do
        result <- runExceptT $ updateFfzEmotes rts channel
        case (result, channel) of
          (Right (), Nothing) ->
            hPutStrLn replHandle "Global FFZ emotes are updated"
          (Right (), Just channelName) ->
            hPutStrLn replHandle $
            "FFZ emotes are updated for channel " <>
            T.unpack (twitchIrcChannelText channelName)
          (Left message, _) -> hPutStrLn replHandle $ "[ERROR] " <> message
      replThreadLoop rts
    ("addrole":name:_, _) -> do
      withTransactionLogErrors $ do
        role <- getTwitchRoleByName rts name
        case role of
          Just _ ->
            hPutStrLn replHandle $ "Role " <> T.unpack name <> " already exists"
          Nothing -> do
            addTwitchRole rts name
            hPutStrLn replHandle $ "Added a new role: " <> T.unpack name
      replThreadLoop rts
    ("lsroles":_, _) -> do
      withTransactionLogErrors $ do
        roles <- listTwitchRoles rts
        mapM_ (hPutStrLn replHandle . T.unpack . twitchRoleName) roles
      replThreadLoop rts
    ("delcmd":name:_, _) -> do
      withTransactionLogErrors $ deleteCommandByName rts name
      replThreadLoop rts
    ("delalias":name:_, _) -> do
      withTransactionLogErrors $ deleteCommandName rts name
      replThreadLoop rts
    ("assrole":roleName:users, _) -> do
      withTransactionLogErrors $
        case rtsTwitchClientId rts of
          Just clientId -> do
            maybeRole <- getTwitchRoleByName rts roleName
            response <-
              HTTP.responseBody <$>
              getUsersByLogins (rtsManager rts) clientId users
            case (response, maybeRole) of
              (Right twitchUsers, Just role') ->
                traverse_
                  (assTwitchRoleToUser rts (twitchRoleId role') . twitchUserId)
                  twitchUsers
              (Left message, _) -> hPutStrLn replHandle $ "[ERROR] " <> message
              (_, Nothing) ->
                hPutStrLn replHandle "[ERROR] Such role does not exist"
          Nothing -> hPutStrLn replHandle "[ERROR] No twitch configuration"
      replThreadLoop rts
    (unknown:_, _) -> do
      hPutStrLn replHandle $ T.unpack $ "Unknown command: " <> unknown
      replThreadLoop rts
    _ -> replThreadLoop rts

data BackdoorThreadParams = BackdoorThreadParams
  { btpChannels :: !(TVar (S.Set TwitchIrcChannel))
  , btpSqliteFileName :: !FilePath
  , btpCommandQueue :: !(WriteQueue ReplCommand)
  , btpTwitchClientId :: Maybe T.Text
  , btpManager :: !HTTP.Manager
  , btpLogQueue :: !(WriteQueue LogEntry)
  , btpPort :: Int
  }

instance ProvidesLogging BackdoorThreadParams where
  logQueue = btpLogQueue

backdoorThread :: BackdoorThreadParams -> IO ()
backdoorThread btp = do
  addr:_ <-
    getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show $ btpPort btp)
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
      logEntry btp $
        LogEntry "BACKDOOR" $
        T.pack (show addr) <> " has connected to the Backdoor gachiBASS"
      connHandle <- socketToHandle conn ReadWriteMode
      replThread $
        ReplThreadParams
          { rtpChannels = btpChannels btp
          , rtpSqliteFileName = btpSqliteFileName btp
          , rtpCommandQueue = btpCommandQueue btp
          , rtpManager = btpManager btp
          , rtpHandle = connHandle
          , rtpLogQueue = btpLogQueue btp
          , rtpConnAddr = addr
          , rtpTwitchClientId = btpTwitchClientId btp
          }
-- TODO(#82): there is no REPL mechanism to update command cooldown

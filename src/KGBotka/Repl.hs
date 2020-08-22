{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.QQ
import KGBotka.Bttv
import KGBotka.Command
import KGBotka.Config
import KGBotka.Ffz
import KGBotka.JoinedTwitchChannels
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Queue
import KGBotka.Roles
import KGBotka.Sqlite
import KGBotka.TwitchAPI
import qualified Network.HTTP.Client as HTTP
import Network.Socket
import System.IO
import System.Random
import Text.Printf

data ReplThreadParams = ReplThreadParams
  { rtpSqliteConnection :: !(MVar Sqlite.Connection)
  , rtpCommandQueue :: !(WriteQueue ReplCommand)
  , rtpConfigTwitch :: !(Maybe ConfigTwitch)
  , rtpManager :: !HTTP.Manager
  , rtpHandle :: !Handle
  , rtpLogQueue :: !(WriteQueue LogEntry)
  , rtpConnAddr :: !SockAddr
  , rtpMarkovQueue :: !(WriteQueue MarkovCommand)
  , rtpRetrainProgress :: !(MVar (Maybe Int))
  }

instance ProvidesLogging ReplThreadParams where
  logEntry rtp = logEntry $ rtpLogQueue rtp

data ReplThreadState = ReplThreadState
  { rtsSqliteConnection :: !(MVar Sqlite.Connection)
  , rtsCurrentChannel :: !(Maybe TwitchIrcChannel)
  , rtsCommandQueue :: !(WriteQueue ReplCommand)
  , rtsConfigTwitch :: !(Maybe ConfigTwitch)
  , rtsManager :: !HTTP.Manager
  , rtsHandle :: !Handle
  , rtsLogQueue :: !(WriteQueue LogEntry)
  , rtsConnAddr :: !SockAddr
  , rtsMarkovQueue :: !(WriteQueue MarkovCommand)
  , rtsRetrainProgress :: !(MVar (Maybe Int))
  }

data ReplCommand
  = Say TwitchIrcChannel
        T.Text
  | JoinChannel TwitchIrcChannel
  | PartChannel TwitchIrcChannel

replThread :: ReplThreadParams -> IO ()
replThread rtp =
  replThreadLoop
    ReplThreadState
      { rtsSqliteConnection = rtpSqliteConnection rtp
      , rtsCurrentChannel = Nothing
      , rtsCommandQueue = rtpCommandQueue rtp
      , rtsConfigTwitch = rtpConfigTwitch rtp
      , rtsManager = rtpManager rtp
      , rtsHandle = rtpHandle rtp
      , rtsLogQueue = rtpLogQueue rtp
      , rtsConnAddr = rtpConnAddr rtp
      , rtsMarkovQueue = rtpMarkovQueue rtp
      , rtsRetrainProgress = rtpRetrainProgress rtp
      }

replPutStr :: Handle -> T.Text -> IO ()
replPutStr h = BS.hPutStr h . T.encodeUtf8

replPutStrLn :: Handle -> T.Text -> IO ()
replPutStrLn h text = replPutStr h $ text <> "\n"

-- TODO(#60): there is no shutdown command that shuts down the whole bot
-- Since we introduce backdoor connections the quit command does
-- not serve such purpose anymore, 'cause it only closes the current
-- REPL connection
-- TODO(#65): there is no `who` command that would show all of the Backdoor connections
replThreadLoop :: ReplThreadState -> IO ()
replThreadLoop rts = do
  let replHandle = rtsHandle rts
  let withTransactionLogErrors :: (Sqlite.Connection -> IO ()) -> IO ()
      withTransactionLogErrors f =
        catch
          (withLockedTransaction (rtsSqliteConnection rts) f)
          (\e -> hPrint replHandle (e :: Sqlite.SQLError))
  replPutStr replHandle $
    "[" <> twitchIrcChannelText (fromMaybe "#" $ rtsCurrentChannel rts) <> "]> "
  hFlush (rtsHandle rts)
  inputLine <- T.decodeUtf8 <$> BS.hGetLine replHandle
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
      replPutStrLn
        replHandle
        "No current channel to say anything to is selected"
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
      atomically $ writeQueue (rtsCommandQueue rts) $ PartChannel channel
      replThreadLoop $ rts {rtsCurrentChannel = Nothing}
    ("ls":_, _) -> do
      withTransactionLogErrors
        (traverse_ (replPutStrLn replHandle . twitchIrcChannelText) <=<
         joinedChannels)
      replThreadLoop rts
    -- TODO(#212): addcmd in REPL should accept the argsRegex
    ("addcmd":name:args, _) -> do
      withTransactionLogErrors $ \dbConn ->
        void $ addCommand dbConn name (T.unwords args)
      replThreadLoop rts
    ("addalias":alias:name:_, _) -> do
      withTransactionLogErrors $ \dbConn -> addCommandName dbConn alias name
      replThreadLoop rts
    ("updatebttv":_, _) -> do
      withTransactionLogErrors $ \dbConn -> do
        let reportFailure =
              \case
                Left message -> hPrintf replHandle "[ERROR] %s\n" message
                Right _ -> return ()
        replPutStrLn replHandle "Updating Global BTTV emotes..."
        reportFailure =<<
          runExceptT (updateBttvEmotes dbConn (rtsManager rts) Nothing)
        channels <- joinedChannels dbConn
        for_ channels $ \channel -> do
          hPrintf replHandle "Updating BTTV emotes for %s channel...\n" $
            twitchIrcChannelText channel
          reportFailure =<<
            runExceptT (updateBttvEmotes dbConn (rtsManager rts) (Just channel))
      replThreadLoop rts
    ("updateffz":_, _) -> do
      withTransactionLogErrors $ \dbConn -> do
        let reportFailure =
              \case
                Left message -> hPrintf replHandle "[ERROR] %s\n" message
                Right _ -> return ()
        hPrintf replHandle "Updating Global FFZ emotes...\n"
        reportFailure =<<
          runExceptT (updateFfzEmotes dbConn (rtsManager rts) Nothing)
        channels <- joinedChannels dbConn
        for_ channels $ \channel -> do
          hPrintf replHandle "Update FFZ emotes for %s channel...\n" $
            twitchIrcChannelText channel
          reportFailure =<<
            runExceptT (updateFfzEmotes dbConn (rtsManager rts) (Just channel))
      replThreadLoop rts
    ("addrole":name:_, _) -> do
      withTransactionLogErrors $ \dbConn -> do
        role <- getTwitchRoleByName dbConn name
        case role of
          Just _ ->
            replPutStrLn replHandle $ "Role " <> name <> " already exists"
          Nothing -> do
            void $ addTwitchRole dbConn name
            replPutStrLn replHandle $ "Added a new role: " <> name
      replThreadLoop rts
    ("lsroles":_, _) -> do
      withTransactionLogErrors $ \dbConn -> do
        roles <- listTwitchRoles dbConn
        mapM_ (replPutStrLn replHandle . twitchRoleName) roles
      replThreadLoop rts
    ("delcmd":name:_, _) -> do
      withTransactionLogErrors $ \dbConn -> deleteCommandByName dbConn name
      replThreadLoop rts
    ("delalias":name:_, _) -> do
      withTransactionLogErrors $ \dbConn -> deleteCommandName dbConn name
      replThreadLoop rts
    ("assrole":roleName:users, _) -> do
      withTransactionLogErrors $ \dbConn ->
        case rtsConfigTwitch rts of
          Just config -> do
            maybeRole <- getTwitchRoleByName dbConn roleName
            response <- getUsersByLogins (rtsManager rts) config users
            case (response, maybeRole) of
              (Right twitchUsers, Just role') ->
                traverse_
                  (assTwitchRoleToUser dbConn (twitchRoleId role') .
                   twitchUserId)
                  twitchUsers
              (Left twitchErr, _) ->
                replPutStrLn replHandle $ "[ERROR] " <> T.pack (show twitchErr)
              (_, Nothing) ->
                replPutStrLn replHandle "[ERROR] Such role does not exist"
          Nothing -> replPutStrLn replHandle "[ERROR] No twitch configuration"
      replThreadLoop rts
    ("retrain":_, _) -> do
      atomically $ writeQueue (rtsMarkovQueue rts) Retrain
      replPutStrLn replHandle "Scheduled Markov retraining..."
      replThreadLoop rts
    ("retrain-stop":_, _) -> do
      atomically $ writeQueue (rtsMarkovQueue rts) StopRetrain
      replPutStrLn replHandle "Retraining process has been stopped..."
      replThreadLoop rts
    ("retrain-pogress":_, _) -> do
      withMVar (rtsRetrainProgress rts) $ \case
        Just progress ->
          withTransactionLogErrors $ \dbConn -> do
            n <-
              maybe (0 :: Int) Sqlite.fromOnly . listToMaybe <$>
              Sqlite.queryNamed dbConn [sql|SELECT count(*) FROM TwitchLog|] []
            replPutStrLn replHandle $
              T.pack $ printf "Current progress: %d/%d" progress n
        Nothing ->
          replPutStrLn replHandle "There is no Markov retraining in place."
      replThreadLoop rts
    ("setprefix":prefix:_, chan) -> do
        withTransactionLogErrors $ \dbConn ->
            case chan of
              Nothing -> replPutStrLn replHandle $ "setprefix only works in a joined channel."
              Just channel -> do
                        setPrefixOfJoinedChannel dbConn channel prefix
                        replPutStrLn replHandle $ "Updated call prefix for channel " <> twitchIrcChannelText channel
        replThreadLoop rts
    (unknown:_, _) -> do
      replPutStrLn replHandle $ "Unknown command: " <> unknown
      replThreadLoop rts
    _ -> replThreadLoop rts

data BackdoorThreadParams = BackdoorThreadParams
  { btpSqliteConnection :: !(MVar Sqlite.Connection)
  , btpCommandQueue :: !(WriteQueue ReplCommand)
  , btpConfigTwitch :: !(Maybe ConfigTwitch)
  , btpManager :: !HTTP.Manager
  , btpLogQueue :: !(WriteQueue LogEntry)
  , btpPort :: !Int
  , btpMarkovQueue :: !(WriteQueue MarkovCommand)
  , btpRetrainProgress :: !(MVar (Maybe Int))
  }

instance ProvidesLogging BackdoorThreadParams where
  logEntry btp = logEntry $ btpLogQueue btp

-- TODO(#231): CSRF token generation is weak
--   - We are not using cryptographic RNG
--   - The size and method of the generation based literally on nothing (some sort CSRF token generation research is required)
csrfToken :: IO T.Text
csrfToken = BS.encodeBase64 . BS.pack <$> replicateM 32 (randomRIO (0, 255))

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
      csrf <- csrfToken
      hPrintf connHandle "CSRF => %s\ncsrf> " csrf
      inputLine <- T.decodeUtf8 <$> BS.hGetLine connHandle
      when (inputLine == csrf) $
        replThread $
        ReplThreadParams
          { rtpSqliteConnection = btpSqliteConnection btp
          , rtpCommandQueue = btpCommandQueue btp
          , rtpManager = btpManager btp
          , rtpHandle = connHandle
          , rtpLogQueue = btpLogQueue btp
          , rtpConnAddr = addr
          , rtpConfigTwitch = btpConfigTwitch btp
          , rtpMarkovQueue = btpMarkovQueue btp
          , rtpRetrainProgress = btpRetrainProgress btp
          }
      hClose connHandle
      close conn
-- TODO(#82): there is no REPL mechanism to update command cooldown

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  , ConfigTwitch(..)
  , recorderThread
  , Recorder(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Aeson
import Data.Foldable
import Data.Functor
import qualified Data.Set as S
import Data.Time
import Data.Traversable
import qualified Database.SQLite.Simple as Sqlite
import Hookup
import Irc.Commands
import Irc.RawIrcMsg
import KGBotka.Bot
import KGBotka.Config
import KGBotka.Migration
import KGBotka.Queue
import KGBotka.Repl
import qualified Network.HTTP.Client.TLS as TLS
import Network.Socket (Family(AF_INET))
import System.Environment
import System.Exit
import System.IO

-- TODO(#2): friday video queue
migrations :: [Migration]
migrations =
  [ "CREATE TABLE Log (\
    \  id INTEGER PRIMARY KEY,\
    \  message TEXT NOT NULL\
    \);"
  , "CREATE TABLE Command (\
    \  id INTEGER PRIMARY KEY,\
    \  code TEXT NOT NULL\
    \);"
  -- TODO(#4): do we need to to cascade delete CommandName-s when the Command is deleted?
  , "CREATE TABLE CommandName (\
    \  name TEXT NOT NULL,\
    \  commandId INTEGER NOT NULL REFERENCES Command(id) ON DELETE CASCADE,\
    \  UNIQUE(name) ON CONFLICT REPLACE\
    \);"
  , "CREATE TABLE TwitchRoles ( \
    \  id INTEGER PRIMARY KEY, \
    \  name TEXT NOT NULL UNIQUE \
    \);"
  , "CREATE TABLE TwitchUserRoles ( \
    \  userId TEXT NOT NULL, \
    \  roleId INTEGER NOT NULL REFERENCES TwitchRoles(id) ON DELETE CASCADE, \
    \  UNIQUE(userId, roleId) ON CONFLICT IGNORE \
    \);"
  ]

maxIrcMessage :: Int
maxIrcMessage = 500 * 4

twitchConnectionParams :: ConnectionParams
twitchConnectionParams =
  ConnectionParams
    { cpHost = "irc.chat.twitch.tv"
    , cpPort = 443
    , cpTls =
        Just
          TlsParams
            { tpClientCertificate = Nothing
            , tpClientPrivateKey = Nothing
            , tpServerCertificate = Nothing
            , tpCipherSuite = "HIGH"
            , tpInsecure = False
            }
    , cpSocks = Nothing
    , cpFamily = AF_INET
    }

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params = bracket (connect params) close

withSqliteConnection :: FilePath -> (Sqlite.Connection -> IO a) -> IO a
withSqliteConnection filePath f =
  bracket (Sqlite.open filePath) Sqlite.close $ \dbConn -> do
    Sqlite.execute_ dbConn "PRAGMA foreign_keys=ON"
    f dbConn

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg conn msg = send conn (renderRawIrcMsg msg)

authorize :: ConfigTwitch -> Connection -> IO ()
authorize conf conn = do
  sendMsg conn (ircPass $ configTwitchToken conf)
  sendMsg conn (ircNick $ configTwitchAccount conf)
  sendMsg conn (ircCapReq ["twitch.tv/tags"])

readIrcLine :: Connection -> IO (Maybe RawIrcMsg)
readIrcLine conn = do
  mb <-
    catch
      (recvLine conn maxIrcMessage)
      (\case
         LineTooLong -> do
           hPutStrLn stderr "[WARN] Received LineTooLong. Ignoring it..."
           return Nothing
         e -> throwIO e)
  for mb $ \xs ->
    case parseRawIrcMsg (asUtf8 xs) of
      Just msg -> return $! msg
      Nothing -> fail "Server sent invalid message!"

twitchIncomingThread :: Connection -> WriteQueue RawIrcMsg -> IO ()
twitchIncomingThread conn queue = do
  mb <- readIrcLine conn
  for_ mb $ atomically . writeQueue queue
  twitchIncomingThread conn queue

twitchOutgoingThread :: Connection -> ReadQueue RawIrcMsg -> IO ()
twitchOutgoingThread conn queue = do
  rawMsg <- atomically $ readQueue queue
  sendMsg conn rawMsg
  twitchOutgoingThread conn queue

data Recorder a = Recorder
  { recorderInput :: !(ReadQueue a)
  , recorderOutput :: !(WriteQueue a)
  , recorderLog :: !(TVar [(UTCTime, a)])
  }

recorderThread :: Recorder a -> IO ()
recorderThread state@Recorder {recorderLog = logs, recorderOutput = outputQueue} = do
  threadDelay 10000 -- to prevent busy looping
  now <- getCurrentTime
  atomically $ do
    maybeInput <- tryReadQueue $ recorderInput state
    case maybeInput of
      Just input -> do
        modifyTVar logs ((now, input) :)
        void $ writeQueue outputQueue input
      Nothing -> return ()
  recorderThread state

newInputRecorder :: ReadQueue a -> STM (Recorder a)
newInputRecorder input = do
  output <- WriteQueue <$> newTQueue
  logs <- newTVar []
  return $ Recorder input output logs

newOutputRecorder :: WriteQueue a -> STM (Recorder a)
newOutputRecorder output = do
  input <- ReadQueue <$> newTQueue
  logs <- newTVar []
  return $ Recorder input output logs

withForkIOs :: [IO ()] -> ([ThreadId] -> IO b) -> IO b
withForkIOs ios = bracket (traverse forkIO ios) (traverse_ killThread)

mainWithArgs :: [String] -> IO ()
mainWithArgs (configPath:databasePath:_) = do
  putStrLn $ "Your configuration file is " <> configPath
  eitherDecodeFileStrict configPath >>= \case
    Right config -> do
      incomingIrcQueue <- atomically newTQueue
      outgoingIrcQueue <- atomically newTQueue
      incomingIrcRecorder <-
        atomically $ newInputRecorder $ ReadQueue incomingIrcQueue
      outgoingIrcRecorder <-
        atomically $ newOutputRecorder $ WriteQueue outgoingIrcQueue
      replQueue <- atomically newTQueue
      joinedChannels <- atomically $ newTVar S.empty
      manager <- TLS.newTlsManager
      withSqliteConnection databasePath $ \dbConn -> do
        Sqlite.withTransaction dbConn $ migrateDatabase dbConn migrations
        withConnection twitchConnectionParams $ \conn -> do
          authorize config conn
          withFile "twitch.log" AppendMode $ \logHandler ->
            withForkIOs
              [ twitchIncomingThread conn $ WriteQueue incomingIrcQueue
              , twitchOutgoingThread conn $ ReadQueue outgoingIrcQueue
              , botThread $
                BotState
                  { botStateIncomingQueue =
                      toReadQueue $ recorderOutput incomingIrcRecorder
                  , botStateOutgoingQueue =
                      toWriteQueue $ recorderInput outgoingIrcRecorder
                  , botStateReplQueue = ReadQueue replQueue
                  , botStateChannels = joinedChannels
                  , botStateSqliteConnection = dbConn
                  , botStateLogHandle = logHandler
                  }
              , recorderThread incomingIrcRecorder
              , recorderThread outgoingIrcRecorder
              ] $ \_ ->
              replThread $
              ReplState
                { replStateChannels = joinedChannels
                , replStateSqliteConnection = dbConn
                , replStateCurrentChannel = Nothing
                , replStateCommandQueue = WriteQueue replQueue
                , replStateConfigTwitch = config
                , replStateManager = manager
                }
      putStrLn "Done"
    Left errorMessage -> error errorMessage
mainWithArgs _ = do
  hPutStrLn stderr "[ERROR] Not enough arguments provided"
  hPutStrLn stderr "Usage: ./kgbotka <config.json> <database.db>"
  exitFailure

main :: IO ()
main = getArgs >>= mainWithArgs

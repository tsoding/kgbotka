{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import System.Environment
import System.Exit
import Hookup
import Network.Socket (Family(AF_INET))
import Control.Exception
import Irc.RawIrcMsg
import Irc.Commands
import Data.Traversable
import Irc.Message
import Data.Foldable
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import qualified Database.SQLite.Simple as Sqlite
import Migration
import qualified Data.Set as S
import Irc.Identifier (Identifier, mkId)
import Control.Monad

migrations :: [Migration]
migrations = [
 "CREATE TABLE Log (\
 \  id INTEGER PRIMARY KEY,\
 \  message TEXT NOT NULL\
 \)"]

maxIrcMessage :: Int
maxIrcMessage = 500 * 4

data ReplCommand
  = Say T.Text T.Text
  | JoinChannel T.Text
  | PartChannel Identifier

newtype WriteQueue a = WriteQueue
  { getWriteQueue :: TQueue a
  }

writeQueue :: WriteQueue a -> a -> STM ()
writeQueue = writeTQueue . getWriteQueue

newtype ReadQueue a = ReadQueue
  { getReadQueue :: TQueue a
  }

readQueue :: ReadQueue a -> STM a
readQueue = readTQueue . getReadQueue

tryReadQueue :: ReadQueue a -> STM (Maybe a)
tryReadQueue = tryReadTQueue . getReadQueue

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
withSqliteConnection filePath = bracket (Sqlite.open filePath) Sqlite.close

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
      Nothing  -> fail "Server sent invalid message!"

data ConfigTwitch = ConfigTwitch
  { configTwitchAccount :: T.Text
  , configTwitchToken :: T.Text
  } deriving (Eq)

instance FromJSON ConfigTwitch where
  parseJSON (Object v) =
    ConfigTwitch <$> v .: "account" <*> v .: "token"
  parseJSON invalid = typeMismatch "Config" invalid

replThread :: TVar (S.Set Identifier) -> WriteQueue ReplCommand -> IO ()
replThread state queue = do
  putStr "> "
  hFlush stdout
  cmd <- words <$> getLine
  case cmd of
    "say":channel:args -> do
      atomically $
        writeQueue queue $ Say (T.pack channel) $ T.pack $ unwords args
      replThread state queue
    "quit":_ -> return ()
    "join":channel:_ -> do
      atomically $ writeQueue queue $ JoinChannel $ T.pack channel
      replThread state queue
    "part":channel:_ -> do
      atomically $ do
        let channelId = mkId $ T.pack channel
        isMember <- S.member channelId <$> readTVar state
        when isMember $ writeQueue queue $ PartChannel channelId
      replThread state queue
    "channels":_ -> do
      readTVarIO state >>= putStrLn . show
      replThread state queue
    unknown:_ -> do
      putStrLn ("Unknown command: " <> unknown)
      replThread state queue
    _ -> replThread state queue


twitchIncomingThread :: Connection -> WriteQueue RawIrcMsg -> IO ()
twitchIncomingThread conn queue = do
  mb <- readIrcLine conn
  for_ mb $ atomically . writeQueue queue
  twitchIncomingThread conn queue

botThread ::
     ReadQueue RawIrcMsg
  -> WriteQueue RawIrcMsg
  -> ReadQueue ReplCommand
  -> TVar (S.Set Identifier)
  -> FilePath
  -> FilePath
  -> IO ()
botThread incomingQueue outgoingQueue replQueue state dbFilePath logFilePath =
  withSqliteConnection dbFilePath $ \dbConn -> do
    Sqlite.withTransaction dbConn $ migrateDatabase dbConn migrations
    withFile logFilePath AppendMode $ \logHandle -> botLoop dbConn logHandle
  where
    botLoop dbConn logHandle = do
      threadDelay 10000 -- to prevent busy looping
      maybeRawMsg <- atomically $ tryReadQueue incomingQueue
      for_ maybeRawMsg $ \rawMsg -> do
        let cookedMsg = cookIrcMsg rawMsg
        hPutStrLn logHandle $ "[TWITCH] " <> show cookedMsg
        hFlush logHandle
        case cookedMsg of
          Ping xs -> atomically $ writeQueue outgoingQueue (ircPong xs)
          Join _ channelId _ ->
            atomically $ modifyTVar state $ S.insert channelId
          Part _ channelId _ ->
            atomically $ modifyTVar state $ S.delete channelId
          _ -> return ()
      maybeReplCommand <- atomically $ tryReadQueue replQueue
      for_ maybeReplCommand $ \case
        Say channel msg ->
          atomically $ writeQueue outgoingQueue $ ircPrivmsg channel msg
        JoinChannel channel ->
          atomically $ writeQueue outgoingQueue $ ircJoin channel Nothing
        PartChannel channelId ->
          atomically $ writeQueue outgoingQueue $ ircPart channelId ""
      botLoop dbConn logHandle

twitchLoggingThread :: Connection -> WriteQueue RawIrcMsg -> FilePath -> IO ()
twitchLoggingThread conn queue filePath =
  withFile filePath AppendMode loggingLoop
  where
    loggingLoop logHandle = do
      mb <- readIrcLine conn
      for_ mb $ \msg -> do
        let cookedMsg = cookIrcMsg msg
        hPutStrLn logHandle $ "[TWITCH] " <> show cookedMsg
        hFlush logHandle
        case cookedMsg of
          (Ping xs) -> atomically $ writeQueue queue (ircPong xs)
          _ -> return ()
      loggingLoop logHandle

twitchOutgoingThread :: Connection -> ReadQueue RawIrcMsg -> IO ()
twitchOutgoingThread conn queue = do
  bm <- atomically $ readQueue queue
  sendMsg conn bm
  twitchOutgoingThread conn queue

mainWithArgs :: [String] -> IO ()
mainWithArgs (configPath:databasePath:_) = do
  putStrLn $ "Your configuration file is " <> configPath
  eitherDecodeFileStrict configPath >>= \case
    Right config -> do
      incomingIrcQueue <- atomically newTQueue
      outgoingIrcQueue <- atomically newTQueue
      replQueue <- atomically newTQueue
      state <- atomically $ newTVar S.empty
      withConnection twitchConnectionParams $ \conn -> do
        authorize config conn
        incomingThreadId <-
          forkIO $ twitchIncomingThread conn (WriteQueue incomingIrcQueue)
        outgoingThreadId <-
          forkIO $ twitchOutgoingThread conn (ReadQueue outgoingIrcQueue)
        botThreadId <-
          forkIO $
          botThread
            (ReadQueue incomingIrcQueue)
            (WriteQueue outgoingIrcQueue)
            (ReadQueue replQueue)
            state
            databasePath
            "twitch.log"
        replThread state (WriteQueue replQueue)
        killThread incomingThreadId
        killThread outgoingThreadId
        killThread botThreadId
    Left errorMessage -> error errorMessage
mainWithArgs _ = do
  hPutStrLn stderr "[ERROR] Not enough arguments provided"
  hPutStrLn stderr "Usage: ./kgbotka <config.json> <database.db>"
  exitFailure

main :: IO ()
main = getArgs >>= mainWithArgs

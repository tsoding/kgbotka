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
import Data.Functor
import Control.Concurrent
import Control.Concurrent.STM

maxIrcMessage :: Int
maxIrcMessage = 500 * 4

data ReplCommand = Say T.Text

newtype WriteQueue a = WriteQueue
  { getWriteQueue :: TQueue a
  }

newtype ReadQueue a = ReadQueue
  { getReadQueue :: TQueue a
  }

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

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg conn msg = send conn (renderRawIrcMsg msg)

authorize :: ConfigTwitch -> Connection -> IO ()
authorize conf conn = do
  sendMsg conn (ircPass $ configTwitchToken conf)
  sendMsg conn (ircNick $ configTwitchAccount conf)
  sendMsg conn (ircJoin "#tsoding" Nothing)
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

replThread :: WriteQueue ReplCommand -> IO ()
replThread queue = do
  putStr "> "
  hFlush stdout
  cmd <- words <$> getLine
  case cmd of
    "say":args ->
      atomically $
      writeTQueue (getWriteQueue queue) $ Say $ T.pack $ unwords args
    unknown:_ -> putStrLn ("Unknown command: " <> unknown)
    _ -> return ()
  replThread queue

twitchIncomingThread :: Connection -> WriteQueue RawIrcMsg -> IO ()
twitchIncomingThread conn queue = do
  mb <- readIrcLine conn
  for_ mb $ atomically . writeTQueue (getWriteQueue queue)
  twitchIncomingThread conn queue

botThread ::
     ReadQueue RawIrcMsg
  -> WriteQueue RawIrcMsg
  -> ReadQueue ReplCommand
  -> FilePath
  -> IO ()
botThread incomingQueue outgoingQueue replQueue filePath =
  withFile filePath AppendMode botLoop
  where
    botLoop logHandle = do
      maybeRawMsg <- atomically $ tryReadTQueue $ getReadQueue incomingQueue
      for_ maybeRawMsg $ \rawMsg -> do
        let cookedMsg = cookIrcMsg rawMsg
        hPutStrLn logHandle $ "[TWITCH] " <> show cookedMsg
        hFlush logHandle
        case cookedMsg of
          (Ping xs) ->
            atomically $ writeTQueue (getWriteQueue outgoingQueue) (ircPong xs)
          _ -> return ()
      maybeReplCommand <- atomically $ tryReadTQueue $ getReadQueue replQueue
      for_ maybeReplCommand $ \case
        Say msg ->
          atomically $
          writeTQueue (getWriteQueue outgoingQueue) $ ircPrivmsg "#tsoding" msg
      botLoop logHandle

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
          (Ping xs) ->
            atomically $ writeTQueue (getWriteQueue queue) (ircPong xs)
          _ -> return ()
      loggingLoop logHandle

twitchOutgoingThread :: Connection -> ReadQueue RawIrcMsg -> IO ()
twitchOutgoingThread conn queue = do
  bm <- atomically $ readTQueue (getReadQueue queue)
  sendMsg conn bm
  twitchOutgoingThread conn queue

mainWithArgs :: [String] -> IO ()
mainWithArgs (configPath:_) = do
  putStrLn $ "Your configuration file is " <> configPath
  eitherDecodeFileStrict configPath >>= \case
    Right config -> do
      incomingIrcQueue <- atomically newTQueue
      outgoingIrcQueue <- atomically newTQueue
      replQueue <- atomically newTQueue
      withConnection twitchConnectionParams $ \conn -> do
        authorize config conn
        void $ forkIO $ twitchIncomingThread conn (WriteQueue incomingIrcQueue)
        void $ forkIO $ twitchOutgoingThread conn (ReadQueue outgoingIrcQueue)
        void $
          forkIO $
          botThread
            (ReadQueue incomingIrcQueue)
            (WriteQueue outgoingIrcQueue)
            (ReadQueue replQueue)
            "twitch.log"
        replThread (WriteQueue replQueue)
    Left errorMessage -> error errorMessage
mainWithArgs _ = do
  hPutStrLn stderr "[ERROR] Configuration file is not provided"
  hPutStrLn stderr "Usage: ./kgbotka <config.json>"
  exitFailure

main :: IO ()
main = getArgs >>= mainWithArgs

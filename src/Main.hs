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

recieveLoop :: ConfigTwitch -> Handle -> Connection -> TQueue RawIrcMsg -> IO ()
recieveLoop config logHandle conn queue = do
  mb <- readIrcLine conn
  for_ mb $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    hPutStrLn logHandle $ "[TWITCH] " <> show cookedMsg
    hFlush logHandle
    case cookedMsg of
      (Ping xs) -> sendMsg conn (ircPong xs)
      -- (Privmsg userInfo target msgText) -> return ()
      _ -> return ()
  bm <- atomically $ tryReadTQueue queue
  for_ bm $ sendMsg conn
  recieveLoop config logHandle conn queue

twitchThread :: ConfigTwitch -> TQueue RawIrcMsg -> IO ()
twitchThread config queue =
  withConnection twitchConnectionParams $ \conn -> do
    withFile "twitch.log" AppendMode $ \logHandle -> do
      authorize config conn
      recieveLoop config logHandle conn queue

replThread :: TQueue RawIrcMsg -> IO ()
replThread queue = do
  putStr "> "
  hFlush stdout
  cmd <- words <$> getLine
  case cmd of
    "say":args ->
      atomically $
      writeTQueue queue $ ircPrivmsg "#tsoding" $ T.pack $ unwords args
    unknown:_ -> putStrLn ("Unknown command: " <> unknown)
    _ -> return ()
  replThread queue

mainWithArgs :: [String] -> IO ()
mainWithArgs (configPath:_) = do
  putStrLn $ "Your configuration file is " <> configPath
  eitherDecodeFileStrict configPath >>= \case
    Right config -> do
      queue <- atomically $ newTQueue
      void $ forkIO $ twitchThread config queue
      replThread queue
    Left errorMessage -> error errorMessage
mainWithArgs _ = do
  hPutStrLn stderr "[ERROR] Configuration file is not provided"
  hPutStrLn stderr "Usage: ./kgbotka <config.json>"
  exitFailure

main :: IO ()
main = getArgs >>= mainWithArgs

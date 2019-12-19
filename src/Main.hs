{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Command
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable
import qualified Database.SQLite.Simple as Sqlite
import Hookup
import Irc.Commands
import Irc.Identifier (Identifier, idText, mkId)
import Irc.Message
import Irc.RawIrcMsg
import Migration
import Network.Socket (Family(AF_INET))
import System.Environment
import System.Exit
import System.IO

-- TODO(#1): link filter
-- TODO(#2): friday video queue
-- TODO(#3): command DSL

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
  -- TODO: do we need to to cascade delete CommandName-s when the Command is deleted?
  , "CREATE TABLE CommandName (\
    \  name TEXT NOT NULL,\
    \  commandId INTEGER NOT NULL REFERENCES Command(id) ON DELETE CASCADE,\
    \  UNIQUE(name) ON CONFLICT REPLACE\
    \);"
  ]

maxIrcMessage :: Int
maxIrcMessage = 500 * 4

data ReplCommand
  = Say T.Text
        T.Text
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
      Nothing -> fail "Server sent invalid message!"

data ConfigTwitch = ConfigTwitch
  { configTwitchAccount :: T.Text
  , configTwitchToken :: T.Text
  } deriving (Eq)

instance FromJSON ConfigTwitch where
  parseJSON (Object v) = ConfigTwitch <$> v .: "account" <*> v .: "token"
  parseJSON invalid = typeMismatch "Config" invalid

replThread ::
     Maybe T.Text
  -> TVar (S.Set Identifier)
  -> Sqlite.Connection
  -> WriteQueue ReplCommand
  -> IO ()
replThread currentChannel state dbConn queue = do
  putStr $ "[" <> T.unpack (fromMaybe "#" currentChannel) <> "]> "
  hFlush stdout
  cmd <- T.words . T.pack <$> getLine
  case (cmd, currentChannel) of
    ("cd":channel:_, _) -> replThread (Just channel) state dbConn queue
    ("cd":_, _) -> replThread Nothing state dbConn queue
    ("say":args, Just channel) -> do
      atomically $ writeQueue queue $ Say channel $ T.unwords args
      replThread currentChannel state dbConn queue
    ("say":_, Nothing) -> do
      putStrLn "No current channel to say anything to is selected"
      replThread currentChannel state dbConn queue
    ("quit":_, _) -> return ()
    ("join":channel:_, _) -> do
      atomically $ writeQueue queue $ JoinChannel channel
      replThread (Just channel) state dbConn queue
    ("part":_, Just channel) -> do
      atomically $ do
        let channelId = mkId channel
        isMember <- S.member channelId <$> readTVar state
        when isMember $ writeQueue queue $ PartChannel channelId
      replThread Nothing state dbConn queue
    ("ls":_, _) -> do
      traverse_ (putStrLn . T.unpack . idText) =<< S.toList <$> readTVarIO state
      replThread currentChannel state dbConn queue
    ("addcmd":name:args, _) -> do
      Sqlite.withTransaction dbConn $ addCommand dbConn name (T.unwords args)
      replThread currentChannel state dbConn queue
    ("addalias":alias:name:_, _) -> do
      Sqlite.withTransaction dbConn $ addCommandName dbConn alias name
      replThread currentChannel state dbConn queue
    ("delcmd":name:_, _) -> do
      Sqlite.withTransaction dbConn $ deleteCommandByName dbConn name
      replThread currentChannel state dbConn queue
    ("delalias":name:_, _) -> do
      Sqlite.withTransaction dbConn $ deleteCommandName dbConn name
      replThread currentChannel state dbConn queue
    (unknown:_, _) -> do
      putStrLn $ T.unpack $ "Unknown command: " <> unknown
      replThread currentChannel state dbConn queue
    _ -> replThread currentChannel state dbConn queue

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
  -> Sqlite.Connection
  -> FilePath
  -> IO ()
botThread incomingQueue outgoingQueue replQueue state dbConn logFilePath =
  withFile logFilePath AppendMode $ \logHandle -> botLoop logHandle
  where
    botLoop logHandle = do
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
          Privmsg _ channelId message ->
            case parseCommandCall "!" message of
              Just (CommandCall name _) -> do
                command <- commandByName dbConn name
                case command of
                  Just (Command _ code) ->
                    atomically $
                    writeQueue outgoingQueue $
                    ircPrivmsg (idText channelId) $ twitchCmdEscape code
                  Nothing -> return ()
              _ -> return ()
          _ -> return ()
      atomically $ do
        replCommand <- tryReadQueue replQueue
        case replCommand of
          Just (Say channel msg) ->
            writeQueue outgoingQueue $ ircPrivmsg channel msg
          Just (JoinChannel channel) ->
            writeQueue outgoingQueue $ ircJoin channel Nothing
          Just (PartChannel channelId) ->
            writeQueue outgoingQueue $ ircPart channelId ""
          Nothing -> return ()
      botLoop logHandle
    twitchCmdEscape :: T.Text -> T.Text
    twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

twitchOutgoingThread :: Connection -> ReadQueue RawIrcMsg -> IO ()
twitchOutgoingThread conn queue = do
  rawMsg <- atomically $ readQueue queue
  sendMsg conn rawMsg
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
      withSqliteConnection databasePath $ \dbConn -> do
        Sqlite.execute_ dbConn "PRAGMA foreign_keys=ON"
        Sqlite.withTransaction dbConn $ migrateDatabase dbConn migrations
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
              dbConn
              "twitch.log"
          replThread Nothing state dbConn (WriteQueue replQueue)
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

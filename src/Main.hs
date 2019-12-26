{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  , ConfigTwitch(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import qualified Data.Map as M
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
import KGBotka.Command
import KGBotka.Expr
import KGBotka.Flip
import KGBotka.Migration
import KGBotka.Parser
import Network.Socket (Family(AF_INET))
import Network.URI
import System.Environment
import System.Exit
import System.IO
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import KGBotka.TwitchAPI

-- TODO(#1): link filter
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
    \  userId INTEGER NOT NULL, \
    \  roleId INTEGER NOT NULL REFERENCES TwitchRoles(id) ON DELETE CASCADE, \
    \  UNIQUE(userId, roleId) \
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

data ConfigTwitch = ConfigTwitch
  { configTwitchAccount :: T.Text
  , configTwitchToken :: T.Text
  , configTwitchClientId :: T.Text
  } deriving (Eq)

instance FromJSON ConfigTwitch where
  parseJSON (Object v) =
    ConfigTwitch <$> v .: "account" <*> v .: "token" <*> v .: "clientId"
  parseJSON invalid = typeMismatch "Config" invalid

data ReplState = ReplState
  { replStateChannels :: TVar (S.Set Identifier)
  , replStateSqliteConnection :: Sqlite.Connection
  , replStateCurrentChannel :: Maybe T.Text
  , replStateCommandQueue :: WriteQueue ReplCommand
  , replStateConfigTwitch :: ConfigTwitch
  , replStateManager :: HTTP.Manager
  }

replThread :: ReplState -> IO ()
replThread state = do
  putStr $
    "[" <> T.unpack (fromMaybe "#" $ replStateCurrentChannel state) <> "]> "
  hFlush stdout
  cmd <- T.words . T.pack <$> getLine
  case (cmd, replStateCurrentChannel state) of
    ("cd":channel:_, _) ->
      replThread $ state {replStateCurrentChannel = Just channel}
    ("cd":_, _) -> replThread $ state {replStateCurrentChannel = Nothing}
    ("say":args, Just channel) -> do
      atomically $
        writeQueue (replStateCommandQueue state) $ Say channel $ T.unwords args
      replThread state
    ("say":_, Nothing) -> do
      putStrLn "No current channel to say anything to is selected"
      replThread state
    ("quit":_, _) -> return ()
    ("join":channel:_, _) -> do
      atomically $
        writeQueue (replStateCommandQueue state) $ JoinChannel channel
      replThread $ state {replStateCurrentChannel = Just channel}
    ("part":_, Just channel) -> do
      atomically $ do
        let channelId = mkId channel
        isMember <- S.member channelId <$> readTVar (replStateChannels state)
        when isMember $
          writeQueue (replStateCommandQueue state) $ PartChannel channelId
      replThread state
    ("ls":_, _) -> do
      traverse_ (putStrLn . T.unpack . idText) =<<
        S.toList <$> readTVarIO (replStateChannels state)
      replThread state
    ("addcmd":name:args, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ addCommand dbConn name (T.unwords args)
      replThread state
    ("addalias":alias:name:_, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ addCommandName dbConn alias name
      replThread state
    ("delcmd":name:_, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ deleteCommandByName dbConn name
      replThread state
    ("delalias":name:_, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ deleteCommandName dbConn name
      replThread state
    ("users":users, _) -> do
      response <-
        HTTP.responseBody . unwrapJsonResponse <$>
        getUsersByLogins
          (replStateManager state)
          (configTwitchClientId $ replStateConfigTwitch state)
          users
      case response of
        Right twitchUsers -> print twitchUsers
        Left message -> putStrLn $ "[ERROR] " <> message
      replThread state
    (unknown:_, _) -> do
      putStrLn $ T.unpack $ "Unknown command: " <> unknown
      replThread state
    _ -> replThread state

twitchIncomingThread :: Connection -> WriteQueue RawIrcMsg -> IO ()
twitchIncomingThread conn queue = do
  mb <- readIrcLine conn
  for_ mb $ atomically . writeQueue queue
  twitchIncomingThread conn queue

evalExpr :: M.Map T.Text T.Text -> Expr -> T.Text
evalExpr _ (TextExpr t) = t
evalExpr vars (FunCallExpr "or" args) =
  fromMaybe "" $ listToMaybe $ dropWhile T.null $ map (evalExpr vars) args
evalExpr vars (FunCallExpr "urlencode" args) =
  T.concat $ map (T.pack . encodeURI . T.unpack . evalExpr vars) args
  where
    encodeURI = escapeURIString (const False)
evalExpr vars (FunCallExpr "flip" args) =
  T.concat $ map (flipText . evalExpr vars) args
evalExpr vars (FunCallExpr funame _) = fromMaybe "" $ M.lookup funame vars

evalExprs :: M.Map T.Text T.Text -> [Expr] -> T.Text
evalExprs vars = T.concat . map (evalExpr vars)

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
              Just (CommandCall name args) -> do
                command <- commandByName dbConn name
                case command of
                  Just (Command _ code) ->
                    let codeAst = snd <$> runParser exprs code
                     in case codeAst of
                          Right codeAst' -> do
                            hPutStrLn logHandle $ "[AST] " <> show codeAst'
                            hFlush logHandle
                            atomically $
                              writeQueue outgoingQueue $
                              ircPrivmsg (idText channelId) $
                              twitchCmdEscape $
                              evalExprs (M.fromList [("1", args)]) codeAst'
                          Left err ->
                            hPutStrLn logHandle $ "[ERROR] " <> show err
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
      joinedChannels <- atomically $ newTVar S.empty
      withSqliteConnection databasePath $ \dbConn -> do
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
              joinedChannels
              dbConn
              "twitch.log"
          manager <- TLS.newTlsManager
          replThread $
            ReplState
              { replStateChannels = joinedChannels
              , replStateSqliteConnection = dbConn
              , replStateCurrentChannel = Nothing
              , replStateCommandQueue = WriteQueue replQueue
              , replStateConfigTwitch = config
              , replStateManager = manager
              }
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

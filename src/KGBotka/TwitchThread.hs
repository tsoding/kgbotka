{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module KGBotka.TwitchThread
  ( twitchThread
  , TwitchThreadParams(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Eval
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Foldable
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple (NamedParam((:=)))
import Hookup
import Irc.Commands
import Irc.Identifier (idText)
import Irc.Message
import Irc.RawIrcMsg
import Irc.UserInfo (userNick)
import KGBotka.Command
import KGBotka.Config
import KGBotka.Eval
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Queue
import KGBotka.Repl
import KGBotka.Roles
import KGBotka.Sqlite
import KGBotka.TwitchAPI
import KGBotka.TwitchLog
import qualified Network.HTTP.Client as HTTP
import Network.Socket (Family(AF_INET))
import Text.Printf

roleOfBadge :: T.Text -> Maybe TwitchBadgeRole
roleOfBadge badge
  | "subscriber" `T.isPrefixOf` badge = Just TwitchSub
  | "vip" `T.isPrefixOf` badge = Just TwitchVip
  | "broadcaster" `T.isPrefixOf` badge = Just TwitchBroadcaster
  | "moderator" `T.isPrefixOf` badge = Just TwitchMod
  | otherwise = Nothing

badgeRolesFromRawIrcMsg :: RawIrcMsg -> [TwitchBadgeRole]
badgeRolesFromRawIrcMsg RawIrcMsg {_msgTags = tags} =
  fromMaybe [] $ do
    badges <- lookupEntryValue "badges" tags
    return $ mapMaybe roleOfBadge $ T.splitOn "," badges

tagEntryPair :: TagEntry -> (T.Text, T.Text)
tagEntryPair (TagEntry name value) = (name, value)

tagEntryName :: TagEntry -> T.Text
tagEntryName = fst . tagEntryPair

tagEntryValue :: TagEntry -> T.Text
tagEntryValue = snd . tagEntryPair

lookupEntryValue :: T.Text -> [TagEntry] -> Maybe T.Text
lookupEntryValue name = fmap tagEntryValue . find ((== name) . tagEntryName)

userIdFromRawIrcMsg :: RawIrcMsg -> Maybe TwitchUserId
userIdFromRawIrcMsg RawIrcMsg {_msgTags = tags} =
  TwitchUserId <$> lookupEntryValue "user-id" tags

data TwitchThreadParams = TwitchThreadParams
  { ttpLogQueue :: !(WriteQueue LogEntry)
  , ttpReplQueue :: !(ReadQueue ReplCommand)
  , ttpSqliteConnection :: !(MVar Sqlite.Connection)
  , ttpManager :: !HTTP.Manager
  , ttpConfig :: Maybe ConfigTwitch
  , ttpFridayGistUpdateRequired :: !(MVar ())
  }

instance ProvidesLogging TwitchThreadParams where
  logQueue = ttpLogQueue

data TwitchThreadState = TwitchThreadState
  { ttsLogQueue :: !(WriteQueue LogEntry)
  , ttsReplQueue :: !(ReadQueue ReplCommand)
  , ttsSqliteConnection :: !(MVar Sqlite.Connection)
  , ttsManager :: !HTTP.Manager
  , ttsConfig :: ConfigTwitch
  , ttsIncomingQueue :: !(ReadQueue RawIrcMsg)
  , ttsOutgoingQueue :: !(WriteQueue RawIrcMsg)
  , ttsFridayGistUpdateRequired :: !(MVar ())
  }

instance ProvidesLogging TwitchThreadState where
  logQueue = ttsLogQueue

withConnection :: ConnectionParams -> (Connection -> IO a) -> IO a
withConnection params = bracket (connect params) close

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

authorize :: ConfigTwitch -> Connection -> IO ()
authorize conf conn = do
  sendMsg conn (ircPass $ configTwitchToken conf)
  sendMsg conn (ircNick $ configTwitchAccount conf)
  sendMsg conn (ircCapReq ["twitch.tv/tags"])

sendMsg :: Connection -> RawIrcMsg -> IO ()
sendMsg conn msg = send conn (renderRawIrcMsg msg)

maxIrcMessage :: Int
maxIrcMessage = 500 * 4

readIrcLine :: ProvidesLogging l => Connection -> l -> IO (Maybe RawIrcMsg)
readIrcLine conn l = do
  mb <-
    catch
      (recvLine conn maxIrcMessage)
      (\case
         LineTooLong -> do
           logEntry l $
             LogEntry "TWITCH" "[WARN] Received LineTooLong. Ignoring it..."
           return Nothing
         e -> throwIO e)
  case (parseRawIrcMsg . asUtf8) =<< mb of
    Just msg -> return (Just msg)
    Nothing -> do
      logEntry l $ LogEntry "TWITCH" "Server sent invalid message!"
      return Nothing

twitchIncomingThread ::
     ProvidesLogging l => Connection -> WriteQueue RawIrcMsg -> l -> IO ()
twitchIncomingThread conn queue l = do
  mb <- readIrcLine conn l
  for_ mb $ atomically . writeQueue queue
  twitchIncomingThread conn queue l

twitchOutgoingThread :: Connection -> ReadQueue RawIrcMsg -> IO ()
twitchOutgoingThread conn queue = do
  rawMsg <- atomically $ readQueue queue
  sendMsg conn rawMsg
  twitchOutgoingThread conn queue

joinedChannels :: Sqlite.Connection -> IO [TwitchIrcChannel]
joinedChannels dbConn = do
  channels <- Sqlite.queryNamed dbConn "SELECT * FROM JoinedTwitchChannels;" []
  return $ map Sqlite.fromOnly channels

registerChannel :: Sqlite.Connection -> TwitchIrcChannel -> IO ()
registerChannel dbConn channel =
  Sqlite.executeNamed
    dbConn
    "INSERT INTO JoinedTwitchChannels (name) VALUES (:channel)"
    [":channel" := channel]

unregisterChannel :: Sqlite.Connection -> TwitchIrcChannel -> IO ()
unregisterChannel dbConn channel =
  Sqlite.executeNamed
    dbConn
    "DELETE FROM JoinedTwitchChannels WHERE name = :channel"
    [":channel" := channel]

twitchThread :: TwitchThreadParams -> IO ()
twitchThread ttp =
  case ttpConfig ttp of
    Just config ->
      withConnection twitchConnectionParams $ \twitchConn -> do
        authorize config twitchConn
        incomingIrcQueue <- atomically newTQueue
        void $
          forkIO $
          twitchIncomingThread twitchConn (WriteQueue incomingIrcQueue) ttp
        outgoingIrcQueue <- atomically newTQueue
        channelsToJoin <-
          withLockedTransaction (ttpSqliteConnection ttp) joinedChannels
        atomically $
          for_ channelsToJoin $ \channel ->
            writeQueue (WriteQueue outgoingIrcQueue) $
            ircJoin (twitchIrcChannelText channel) Nothing
        void $
          forkIO $ twitchOutgoingThread twitchConn $ ReadQueue outgoingIrcQueue
        twitchThreadLoop
          TwitchThreadState
            { ttsLogQueue = ttpLogQueue ttp
            , ttsReplQueue = ttpReplQueue ttp
            , ttsSqliteConnection = ttpSqliteConnection ttp
            , ttsManager = ttpManager ttp
            , ttsConfig = config
            , ttsIncomingQueue = ReadQueue incomingIrcQueue
            , ttsOutgoingQueue = WriteQueue outgoingIrcQueue
            , ttsFridayGistUpdateRequired = ttpFridayGistUpdateRequired ttp
            }
    Nothing ->
      atomically $
      writeQueue (ttpLogQueue ttp) $
      LogEntry "TWITCH" "[ERROR] Twitch configuration not found"

countForbidden :: T.Text -> Int
countForbidden = T.length . T.filter (not . isAllowed)

isAllowed :: Char -> Bool
isAllowed = getAny . foldMap (Any .) [isAlpha, isNumber, isSpace, isPunctuation]

processControlMsgs :: TwitchThreadState -> [RawIrcMsg] -> IO ()
processControlMsgs tts messages = do
  let outgoingQueue = ttsOutgoingQueue tts
  for_ messages $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    case cookedMsg of
      Ping xs -> atomically $ writeQueue outgoingQueue (ircPong xs)
      Join _ channelId _ ->
        withLockedTransaction (ttsSqliteConnection tts) $ \dbConn ->
          registerChannel dbConn $ TwitchIrcChannel channelId
      Part _ channelId _ ->
        withLockedTransaction (ttsSqliteConnection tts) $ \dbConn ->
          unregisterChannel dbConn $ TwitchIrcChannel channelId
      _ -> return ()

processUserMsgs ::
     Sqlite.Connection -> TwitchThreadState -> [RawIrcMsg] -> IO ()
processUserMsgs dbConn tts messages = do
  let outgoingQueue = ttsOutgoingQueue tts
  let manager = ttsManager tts
  let botLogin = configTwitchAccount $ ttsConfig tts
  for_ messages $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    logEntry tts $ LogEntry "TWITCH" $ T.pack $ show cookedMsg
    case cookedMsg of
      Privmsg userInfo channelId message ->
        case userIdFromRawIrcMsg msg of
          Just senderId -> do
            roles <- getTwitchUserRoles dbConn senderId
            let badgeRoles = badgeRolesFromRawIrcMsg msg
            let displayName = lookupEntryValue "display-name" $ _msgTags msg
            let senderName = idText $ userNick userInfo
            if T.toLower senderName /= T.toLower botLogin
              then do
                logMessage
                  dbConn
                  (TwitchIrcChannel channelId)
                  senderId
                  senderName
                  displayName
                  roles
                  badgeRoles
                  message
                addMarkovSentence dbConn message
                let forbiddenCharLimit = 100
                if countForbidden message < forbiddenCharLimit
                  then case parseCommandPipe
                              (CallPrefix "$")
                              (PipeSuffix "|")
                              message of
                         [] ->
                           when
                             (T.toUpper (configTwitchAccount $ ttsConfig tts) `T.isInfixOf`
                              T.toUpper message) $ do
                             markovResponse <- genMarkovSentence dbConn
                             atomically $
                               writeQueue outgoingQueue $
                               ircPrivmsg (idText channelId) $
                               twitchCmdEscape $
                               T.pack $
                               printf "@%s %s" senderName markovResponse
                         pipe -> do
                           day <- utctDay <$> getCurrentTime
                           let (yearNum, monthNum, dayNum) = toGregorian day
                           evalResult <-
                             runExceptT $
                             evalStateT (runEvalT $ evalCommandPipe pipe) $
                             EvalContext
                               { ecVars =
                                   M.fromList
                                     [ ("sender", senderName)
                                     , ("year", T.pack $ show yearNum)
                                     , ("month", T.pack $ show monthNum)
                                     , ("day", T.pack $ show dayNum)
                                     , ("date", T.pack $ showGregorian day)
                                     -- TODO(#144): %times var is not available
                                     , ("times", "0")
                                     ]
                               , ecSqliteConnection = dbConn
                               , ecPlatformContext =
                                   Etc
                                     EvalTwitchContext
                                       { etcSenderId = senderId
                                       , etcSenderName = senderName
                                       , etcChannel = TwitchIrcChannel channelId
                                       , etcBadgeRoles = badgeRoles
                                       , etcRoles = roles
                                       , etcClientId =
                                           configTwitchClientId $ ttsConfig tts
                                       , etcTwitchEmotes =
                                           do emotesTag <-
                                                lookupEntryValue "emotes" $
                                                _msgTags msg
                                              if not $ T.null emotesTag
                                                then do
                                                  emoteDesc <-
                                                    listToMaybe $
                                                    T.splitOn "/" emotesTag
                                                  listToMaybe $
                                                    T.splitOn ":" emoteDesc
                                                else Nothing
                                       }
                               , ecLogQueue = logQueue tts
                               , ecManager = manager
                               , ecFridayGistUpdateRequired =
                                   ttsFridayGistUpdateRequired tts
                               }
                           atomically $
                             case evalResult of
                               Right commandResponse ->
                                 writeQueue outgoingQueue $
                                 ircPrivmsg (idText channelId) $
                                 twitchCmdEscape commandResponse
                               Left (EvalError userMsg) ->
                                 writeQueue outgoingQueue $
                                 ircPrivmsg (idText channelId) $
                                 twitchCmdEscape userMsg
                  else atomically $ do
                         writeQueue outgoingQueue $
                           ircPrivmsg (idText channelId) $
                           T.pack $
                           printf "/timeout %s %d" senderName (30 :: Int)
                         writeQueue outgoingQueue $
                           ircPrivmsg (idText channelId) $
                           T.pack $
                           printf
                             "@%s ASCII spam is not allowed. Use !asciify command."
                             senderName
              else logEntry tts $
                   LogEntry "TWITCH" "WARNING: Bot received its own message"
          Nothing ->
            logEntry tts $
            LogEntry "TWITCH" $
            "ERROR: Could not extract twitch user id from PRIVMSG " <>
            T.pack (show msg)
      _ -> return ()

twitchThreadLoop :: TwitchThreadState -> IO ()
twitchThreadLoop tts = do
  threadDelay 10000 -- to prevent busy looping
  let incomingQueue = ttsIncomingQueue tts
  messages <- atomically $ flushQueue incomingQueue
  let (userMessages, controlMessages) =
        partition (\x -> _msgCommand x == "PRIVMSG") messages
  processControlMsgs tts controlMessages
  catch
    (withMVar (ttsSqliteConnection tts) $ \dbConn ->
       Sqlite.withTransaction dbConn $ processUserMsgs dbConn tts userMessages)
    (\e ->
       atomically $
       writeQueue (ttsLogQueue tts) $
       LogEntry "SQLITE" $ T.pack $ show (e :: Sqlite.SQLError))
  atomically $ do
    let outgoingQueue = ttsOutgoingQueue tts
    let replQueue = ttsReplQueue tts
    replCommand <- tryReadQueue replQueue
    case replCommand of
      Just (Say channel msg) ->
        writeQueue outgoingQueue $ ircPrivmsg (twitchIrcChannelText channel) msg
      Just (JoinChannel channel) ->
        writeQueue outgoingQueue $
        ircJoin (twitchIrcChannelText channel) Nothing
      Just (PartChannel (TwitchIrcChannel channelId)) ->
        writeQueue outgoingQueue $ ircPart channelId ""
      Nothing -> return ()
  twitchThreadLoop tts

twitchCmdEscape :: T.Text -> T.Text
twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

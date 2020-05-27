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
import qualified Database.SQLite.Simple as Sqlite
import Hookup
import Irc.Commands
import Irc.Identifier (idText)
import Irc.Message
import Irc.RawIrcMsg
import Irc.UserInfo (userNick)
import KGBotka.Command
import KGBotka.Config
import KGBotka.Eval
import KGBotka.JoinedTwitchChannels
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Queue
import KGBotka.Repl
import KGBotka.Roles
import KGBotka.Settings
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
  | "founder" `T.isPrefixOf` badge = Just TwitchFounder
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
  , ttpConfig :: !(Maybe ConfigTwitch)
  , ttpFridayGistUpdateRequired :: !(MVar ())
  , ttpMarkovQueue :: !(WriteQueue MarkovCommand)
  }

instance ProvidesLogging TwitchThreadParams where
  logEntry ttp = logEntry $ ttpLogQueue ttp

data TwitchThreadState = TwitchThreadState
  { ttsLogQueue :: !(WriteQueue LogEntry)
  , ttsReplQueue :: !(ReadQueue ReplCommand)
  , ttsSqliteConnection :: !(MVar Sqlite.Connection)
  , ttsManager :: !HTTP.Manager
  , ttsConfig :: ConfigTwitch
  , ttsIncomingQueue :: !(ReadQueue RawIrcMsg)
  , ttsOutgoingQueue :: !(WriteQueue OutMsg)
  , ttsFridayGistUpdateRequired :: !(MVar ())
  , ttsMarkovQueue :: !(WriteQueue MarkovCommand)
  }

instance ProvidesLogging TwitchThreadState where
  logEntry tts = logEntry $ ttsLogQueue tts

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
  sendMsg conn (ircPass $ "oauth:" <> configTwitchToken conf)
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
    Nothing -> return Nothing

twitchIncomingThread ::
     ProvidesLogging l => Connection -> WriteQueue RawIrcMsg -> l -> IO ()
twitchIncomingThread conn queue l = do
  mb <- readIrcLine conn l
  for_ mb $ atomically . writeQueue queue
  twitchIncomingThread conn queue l

data OutMsg
  = OutPrivMsg TwitchIrcChannel
               T.Text
  | OutJoinMsg TwitchIrcChannel
  | OutPartMsg TwitchIrcChannel
  | OutPongMsg [T.Text]

renderOutMsg :: OutMsg -> RawIrcMsg
renderOutMsg (OutPrivMsg (TwitchIrcChannel channel) message) =
  ircPrivmsg (idText channel) message
renderOutMsg (OutJoinMsg channel) =
  ircJoin (twitchIrcChannelText channel) Nothing
renderOutMsg (OutPartMsg (TwitchIrcChannel channelId)) = ircPart channelId ""
renderOutMsg (OutPongMsg xs) = ircPong xs

twitchLimitFilter :: OutMsg -> OutMsg
twitchLimitFilter (OutPrivMsg channel message) =
  OutPrivMsg channel (T.take 500 message)
twitchLimitFilter x = x

twitchOutgoingThread :: Connection -> ReadQueue OutMsg -> IO ()
twitchOutgoingThread conn queue = do
  msg <- atomically $ readQueue queue
  sendMsg conn $ renderOutMsg $ twitchLimitFilter msg
  twitchOutgoingThread conn queue

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
            writeQueue (WriteQueue outgoingIrcQueue) $ OutJoinMsg channel
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
            , ttsMarkovQueue = ttpMarkovQueue ttp
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
      Ping xs -> atomically $ writeQueue outgoingQueue $ OutPongMsg xs
      Join _ channelId _ ->
        withLockedTransaction (ttsSqliteConnection tts) $ \dbConn ->
          registerJoinedChannel dbConn $ TwitchIrcChannel channelId
      Part _ channelId _ ->
        withLockedTransaction (ttsSqliteConnection tts) $ \dbConn ->
          unregisterJoinedChannel dbConn $ TwitchIrcChannel channelId
      _ -> return ()

processUserMsgs ::
     Sqlite.Connection -> TwitchThreadState -> [RawIrcMsg] -> IO ()
processUserMsgs dbConn tts messages = do
  let outgoingQueue = ttsOutgoingQueue tts
  let manager = ttsManager tts
  let botLogin = configTwitchAccount $ ttsConfig tts
  for_ messages $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    logEntry tts $ LogEntry "TWITCH" $ T.pack $ show msg
    case cookedMsg of
      Privmsg userInfo channelId message ->
        case userIdFromRawIrcMsg msg of
          Just senderId -> do
            roles <- getTwitchUserRoles dbConn senderId
            let badgeRoles = badgeRolesFromRawIrcMsg msg
            let displayName = lookupEntryValue "display-name" $ _msgTags msg
            let senderName = idText $ userNick userInfo
            let channel = TwitchIrcChannel channelId
            if T.toLower senderName /= T.toLower botLogin
              then do
                logMessage
                  dbConn
                  channel
                  senderId
                  senderName
                  displayName
                  roles
                  badgeRoles
                  message
                atomically $
                  writeQueue (ttsMarkovQueue tts) $ NewSentence message
                let forbiddenCharLimit = 100
                if countForbidden message < forbiddenCharLimit
                  then do
                    settings <- fetchSettings dbConn
                    case parseCommandPipe
                           (settingsCallPrefix settings)
                           (PipeSuffix "|")
                           message of
                      [] ->
                        when
                          (T.toUpper (configTwitchAccount $ ttsConfig tts) `T.isInfixOf`
                           T.toUpper message) $ do
                          markovResponse <- genMarkovSentence dbConn
                          atomically $
                            writeQueue outgoingQueue $
                            OutPrivMsg channel $
                            twitchCmdEscape $
                            T.pack $ printf "@%s %s" senderName markovResponse
                      pipe -> do
                        evalResult <-
                          runExceptT $
                          evalStateT (runEvalT $ evalCommandPipe pipe) $
                          EvalContext
                            { ecVars = M.empty
                            , ecSqliteConnection = dbConn
                            , ecPlatformContext =
                                Etc
                                  EvalTwitchContext
                                    { etcSenderId = senderId
                                    , etcSenderName = senderName
                                    , etcChannel = channel
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
                            , ecLogQueue = ttsLogQueue tts
                            , ecManager = manager
                            , ecFridayGistUpdateRequired =
                                ttsFridayGistUpdateRequired tts
                            }
                        atomically $
                          case evalResult of
                            Right commandResponse ->
                              writeQueue outgoingQueue $
                              OutPrivMsg channel $
                              twitchCmdEscape commandResponse
                            Left (EvalError userMsg) ->
                              writeQueue outgoingQueue $
                              OutPrivMsg channel $ twitchCmdEscape userMsg
                  else atomically $ do
                         writeQueue outgoingQueue $
                           OutPrivMsg channel $
                           T.pack $
                           printf "/timeout %s %d" senderName (30 :: Int)
                         writeQueue outgoingQueue $
                           OutPrivMsg channel $
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
       LogEntry "SQLITE" $ T.pack $ show (e :: SomeException))
  atomically $ do
    let outgoingQueue = ttsOutgoingQueue tts
    let replQueue = ttsReplQueue tts
    replCommand <- tryReadQueue replQueue
    case replCommand of
      Just (Say channel msg) ->
        writeQueue outgoingQueue $ OutPrivMsg channel msg
      Just (JoinChannel channel) ->
        writeQueue outgoingQueue $ OutJoinMsg channel
      Just (PartChannel channel) ->
        writeQueue outgoingQueue $ OutPartMsg channel
      Nothing -> return ()
  twitchThreadLoop tts

twitchCmdEscape :: T.Text -> T.Text
twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

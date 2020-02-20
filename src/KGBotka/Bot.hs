{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Bot
  ( botThread
  , BotState(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Trans.Eval
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import Irc.Commands
import Irc.Identifier (idText)
import Irc.Message
import Irc.RawIrcMsg
import Irc.UserInfo (userNick)
import KGBotka.Command
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Queue
import KGBotka.Repl
import KGBotka.Roles
import KGBotka.Sqlite
import KGBotka.TwitchAPI
import qualified Network.HTTP.Client as HTTP
import System.IO
import KGBotka.Eval

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

data BotState = BotState
  { botStateIncomingQueue :: !(ReadQueue RawIrcMsg)
  , botStateOutgoingQueue :: !(WriteQueue RawIrcMsg)
  , botStateReplQueue :: !(ReadQueue ReplCommand)
  , botStateChannels :: !(TVar (S.Set TwitchIrcChannel))
  , botStateSqliteFileName :: !FilePath
  , botStateLogHandle :: !Handle
  , botStateManager :: !HTTP.Manager
  }

botThread :: BotState -> IO ()
botThread botState = do
  let databaseFileName = botStateSqliteFileName botState
  withConnectionAndPragmas databaseFileName $ \conn -> botThread' conn botState

processControlMsgs :: [RawIrcMsg] -> BotState -> IO ()
processControlMsgs messages botState = do
  let outgoingQueue = botStateOutgoingQueue botState
  let channels = botStateChannels botState
  for_ messages $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    case cookedMsg of
      Ping xs -> atomically $ writeQueue outgoingQueue (ircPong xs)
      Join _ channelId _ ->
        atomically $ modifyTVar channels $ S.insert $ TwitchIrcChannel channelId
      Part _ channelId _ ->
        atomically $ modifyTVar channels $ S.delete $ TwitchIrcChannel channelId
      _ -> return ()

processUserMsgs :: Sqlite.Connection -> [RawIrcMsg] -> BotState -> IO ()
processUserMsgs dbConn messages botState = do
  let outgoingQueue = botStateOutgoingQueue botState
  let logHandle = botStateLogHandle botState
  let manager = botStateManager botState
  for_ messages $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    hPutStrLn logHandle $ "[TWITCH] " <> show msg
    hFlush logHandle
    case cookedMsg of
      Privmsg userInfo channelId message ->
        case userIdFromRawIrcMsg msg of
          Just senderId -> do
            roles <- getTwitchUserRoles dbConn senderId
            let badgeRoles = badgeRolesFromRawIrcMsg msg
            let displayName = lookupEntryValue "display-name" $ _msgTags msg
            logMessage
              dbConn
              (TwitchIrcChannel channelId)
              senderId
              (idText $ userNick userInfo)
              displayName
              roles
              badgeRoles
              message
            addMarkovSentence dbConn message
              -- FIXME(#31): Link filtering is not disablable
            evalResult <-
              runExceptT $
              evalStateT
                (runEvalT $ evalCommandPipe $ parseCommandPipe "!" "|" message) $
              EvalContext
                { evalContextVars =
                    M.fromList [("sender", idText (userNick userInfo))]
                , evalContextSqliteConnection = dbConn
                , evalContextSenderId = senderId
                , evalContextSenderName = idText (userNick userInfo)
                , evalContextChannel = TwitchIrcChannel channelId
                , evalContextBadgeRoles = badgeRoles
                , evalContextRoles = roles
                , evalContextLogHandle = logHandle
                , evalContextTwitchEmotes =
                    do emotesTag <- lookupEntryValue "emotes" $ _msgTags msg
                       if not $ T.null emotesTag
                         then do
                           emoteDesc <- listToMaybe $ T.splitOn "/" emotesTag
                           listToMaybe $ T.splitOn ":" emoteDesc
                         else Nothing
                , evalContextManager = manager
                }
            atomically $
              case evalResult of
                Right commandResponse ->
                  writeQueue outgoingQueue $
                  ircPrivmsg (idText channelId) $
                  twitchCmdEscape commandResponse
                Left (EvalError userMsg) ->
                  writeQueue outgoingQueue $
                  ircPrivmsg (idText channelId) $ twitchCmdEscape userMsg
          Nothing ->
            hPutStrLn logHandle $
            "[WARNING] Could not extract twitch user id from PRIVMSG " <>
            show msg
      _ -> return ()

botThread' :: Sqlite.Connection -> BotState -> IO ()
botThread' dbConn botState = do
  threadDelay 10000 -- to prevent busy looping
  let incomingQueue = botStateIncomingQueue botState
  messages <- atomically $ flushQueue incomingQueue
  let (userMessages, controlMessages) =
        partition (\x -> _msgCommand x == "PRIVMSG") messages
  processControlMsgs controlMessages botState
  catch
    (Sqlite.withTransaction dbConn $
     processUserMsgs dbConn userMessages botState)
    (\e -> hPrint (botStateLogHandle botState) (e :: Sqlite.SQLError))
  atomically $ do
    let outgoingQueue = botStateOutgoingQueue botState
    let replQueue = botStateReplQueue botState
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
  botThread' dbConn botState

twitchCmdEscape :: T.Text -> T.Text
twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

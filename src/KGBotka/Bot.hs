{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module KGBotka.Bot
  ( botThread
  , BotState(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Extra
import Control.Monad.Trans.State
import Data.Array
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import Irc.Commands
import Irc.Identifier (Identifier, idText)
import Irc.Message
import Irc.RawIrcMsg
import Irc.UserInfo (userNick)
import KGBotka.Command
import KGBotka.Expr
import KGBotka.Flip
import KGBotka.Friday
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Parser
import KGBotka.Queue
import KGBotka.Repl
import KGBotka.Roles
import KGBotka.Sqlite
import KGBotka.TwitchAPI
import qualified Network.HTTP.Client as HTTP
import Network.URI
import System.IO
import Text.Printf
import qualified Text.Regex.Base.RegexLike as Regex
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String
import KGBotka.Asciify

data EvalContext = EvalContext
  { evalContextVars :: M.Map T.Text T.Text
  , evalContextSqliteConnection :: Sqlite.Connection
  , evalContextSenderId :: Maybe TwitchUserId
  , evalContextSenderName :: T.Text
  -- TODO: evalContextTwitchEmotes should be a list of some kind of emote type
  , evalContextTwitchEmotes :: Maybe T.Text
  , evalContextChannel :: TwitchIrcChannel
  , evalContextBadgeRoles :: [TwitchBadgeRole]
  , evalContextRoles :: [TwitchRole]
  , evalContextLogHandle :: Handle
  , evalContextManager :: HTTP.Manager
  }

evalContextVarsModify ::
     (M.Map T.Text T.Text -> M.Map T.Text T.Text) -> EvalContext -> EvalContext
evalContextVarsModify f context =
  context {evalContextVars = f $ evalContextVars context}

ytLinkRegex :: Either String Regex
ytLinkRegex =
  compile
    defaultCompOpt
    defaultExecOpt
    "https?:\\/\\/(www\\.)?youtu(be\\.com\\/watch\\?v=|\\.be\\/)([a-zA-Z0-9_-]+)"

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

-- | Extracts YouTube Video ID from the string
-- Results:
-- - `Right ytId` - extracted successfully
-- - `Left (Just failReason)` - extraction failed because of
--    the application's fault. The reason explained in `failReason`.
--    `failReason` should be logged and later investigated by the devs.
--    `failReason` should not be shown to the users.
-- - `Left Nothing` - extraction failed because of the user's fault.
--    Tell the user that their message does not contain any YouTube
--    links.
ytLinkId :: T.Text -> Either (Maybe String) T.Text
ytLinkId text = do
  regex <- mapLeft Just ytLinkRegex
  result <- mapLeft Just $ execute regex (T.unpack text)
  case result of
    Just matches ->
      case map (T.pack . flip Regex.extract (T.unpack text)) $ elems matches of
        [_, _, _, ytId] -> Right ytId
        _ ->
          Left $
          Just
            "Matches were not captured correctly. \
            \Most likely somebody changed the YouTube \
            \link regular expression (`ytLinkRegex`) and didn't \
            \update `ytLinkId` function to extract capture \
            \groups correctly. ( =_=)"
    Nothing -> Left Nothing

newtype EvalError =
  EvalError T.Text
  deriving (Show)

evalExpr :: Expr -> EvalT T.Text
evalExpr (TextExpr t) = return t
evalExpr (FunCallExpr "or" args) =
  fromMaybe "" . listToMaybe . dropWhile T.null <$> mapM evalExpr args
evalExpr (FunCallExpr "urlencode" args) =
  T.concat . map (T.pack . encodeURI . T.unpack) <$> mapM evalExpr args
  where
    encodeURI = escapeURIString (const False)
evalExpr (FunCallExpr "markov" _) = do
  dbConn <- evalContextSqliteConnection <$> get
  logHandle <- evalContextLogHandle <$> get
  events <- lift $ lift $ seqMarkovEvents Begin End dbConn
  lift $ lift $ hPutStrLn logHandle $ "[MARKOV] " <> show events
  return $
    T.unwords $
    mapMaybe
      (\case
         Begin -> Nothing
         End -> Nothing
         Word x -> Just x)
      events
evalExpr (FunCallExpr "flip" args) =
  T.concat . map flipText <$> mapM evalExpr args
-- FIXME(#18): Friday video list is not published on gist
-- FIXME(#38): %nextvideo does not inform how many times a video was suggested
evalExpr (FunCallExpr "nextvideo" _) = do
  badgeRoles <- evalContextBadgeRoles <$> get
  if TwitchBroadcaster `elem` badgeRoles
    then do
      dbConn <- evalContextSqliteConnection <$> get
      channel <- evalContextChannel <$> get
      fridayVideo <-
        lift $
        maybeToExceptT (EvalError "Video queue is empty") $
        nextVideo dbConn channel
      return $ fridayVideoAsMessage fridayVideo
    else lift $ throwE $ EvalError "Only for mr strimmer :)"
  where
    fridayVideoAsMessage :: FridayVideo -> T.Text
    fridayVideoAsMessage FridayVideo { fridayVideoSubText = subText
                                     , fridayVideoSubTime = subTime
                                     , fridayVideoAuthorTwitchName = authorTwitchName
                                     } =
      T.pack (show subTime) <> " <" <> authorTwitchName <> "> " <> subText
-- FIXME(#39): %friday does not inform how many times a video was suggested
evalExpr (FunCallExpr "friday" args) = do
  roles <- evalContextRoles <$> get
  badgeRoles <- evalContextBadgeRoles <$> get
  when (null roles && null badgeRoles) $
    lift $ throwE $ EvalError "You have to be trusted to submit Friday videos"
  submissionText <- T.concat <$> mapM evalExpr args
  case ytLinkId submissionText of
    Right _ -> do
      sender <- evalContextSenderId <$> get
      case sender of
        Just senderId -> do
          dbConn <- evalContextSqliteConnection <$> get
          channel <- evalContextChannel <$> get
          senderName <- evalContextSenderName <$> get
          lift $
            lift $ submitVideo dbConn submissionText channel senderId senderName
          return "Added your video to suggestions"
        Nothing ->
          lift $
          throwE $
          EvalError
            "Sender not found. \
            \It's need for submitting Friday videos"
    Left Nothing ->
      lift $ throwE $ EvalError "Your suggestion should contain YouTube link"
    Left (Just failReason) -> do
      logHandle <- evalContextLogHandle <$> get
      lift $
        lift $
        hPutStrLn logHandle $
        "An error occured while parsing YouTube link: " <> failReason
      lift $
        throwE $
        EvalError
          "Something went wrong while parsing your subsmission. \
          \We are already looking into it. Kapp"
-- TODO(#69): %asciify does not support FFZ emotes
-- TODO(#70): %asciify does not support BTTV emotes
-- TODO(#71): %asciify does not have a cooldown
-- TODO(#72): %asciify does not have trusted filter
evalExpr (FunCallExpr "asciify" _) = do
  roles <- evalContextRoles <$> get
  badgeRoles <- evalContextBadgeRoles <$> get
  when (null roles && null badgeRoles) $
    throwEvalError $ EvalError "Only for trusted users"
  emotes <- evalContextTwitchEmotes <$> get
  lift $ lift $ putStrLn "------------------------------"
  lift $ lift $ print emotes
  lift $ lift $ putStrLn "------------------------------"
  case emotes of
    Just emote -> do
      manager <- evalContextManager <$> get
      dbConn <- evalContextSqliteConnection <$> get
      image <-
        lift $
        lift $
        runExceptT $
        asciifyUrl dbConn manager $
        "https://static-cdn.jtvnw.net/emoticons/v1/" <> emote <> "/3.0"
      case image of
        Right image' -> return image'
        Left errorMessage -> do
          logHandle <- evalContextLogHandle <$> get
          senderName <- evalContextSenderName <$> get
          lift $ lift $ hPutStrLn logHandle errorMessage
          throwEvalError $
            EvalError
              ("@" <> senderName <> " Could not load emote")
    _ -> return ""
evalExpr (FunCallExpr funame _) = do
  vars <- evalContextVars <$> get
  lift $
    maybeToExceptT (EvalError $ "Function `" <> funame <> "` does not exists") $
    hoistMaybe $ M.lookup funame vars

evalExprs :: [Expr] -> EvalT T.Text
evalExprs exprs' = T.concat <$> mapM evalExpr exprs'

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
  , botStateChannels :: !(TVar (S.Set Identifier))
  , botStateSqliteFileName :: !FilePath
  , botStateLogHandle :: !Handle
  , botStateManager :: !HTTP.Manager
  }

type EvalT = StateT EvalContext (ExceptT EvalError IO)

throwEvalError :: EvalError -> StateT EvalContext (ExceptT EvalError IO) a
throwEvalError = lift . throwE

evalCommandCall :: CommandCall -> EvalT T.Text
evalCommandCall (CommandCall name args) = do
  modify $ evalContextVarsModify $ M.insert "1" args
  dbConn <- evalContextSqliteConnection <$> get
  command <- lift $ lift $ commandByName dbConn name
  maybeTwitchUserId <- evalContextSenderId <$> get
  senderName <- evalContextSenderName <$> get
  case command of
    Just Command {commandId = ident, commandCode = code} -> do
      case maybeTwitchUserId of
        Just twitchUserId' -> do
          cooledDown <-
            lift $ lift $ isCommandCooleddown dbConn twitchUserId' ident
          unless cooledDown $
            throwEvalError $
            EvalError $
            "@" <> senderName <> " The command has not cooled down yet"
          lift $ lift $ logCommand dbConn twitchUserId' ident args
        Nothing ->
          throwEvalError $
          EvalError $
          T.pack $
          printf
            "Command %s(%d) with args `%s` was \
            \called without twitch user id"
            name
            ident
            args
      codeAst <-
        lift $
        withExceptT (EvalError . T.pack . show) $
        except (snd <$> runParser exprs code)
      evalExprs codeAst
    Nothing -> return ""

evalCommandPipe :: [CommandCall] -> EvalT T.Text
evalCommandPipe =
  foldlM (\args -> evalCommandCall . ccArgsModify (`T.append` args)) ""

botThread :: BotState -> IO ()
botThread initState =
  withConnectionAndPragmas (botStateSqliteFileName initState) $ \conn ->
    botThread' conn initState

botThread' :: Sqlite.Connection -> BotState -> IO ()
botThread' dbConn botState@BotState { botStateIncomingQueue = incomingQueue
                                    , botStateOutgoingQueue = outgoingQueue
                                    , botStateReplQueue = replQueue
                                    , botStateChannels = channels
                                    , botStateLogHandle = logHandle
                                    , botStateManager = manager
                                    } = do
  threadDelay 10000 -- to prevent busy looping
  maybeRawMsg <- atomically $ flushQueue incomingQueue
  Sqlite.withTransaction dbConn $
    for_ maybeRawMsg $ \rawMsg -> do
      let cookedMsg = cookIrcMsg rawMsg
      hPutStrLn logHandle $ "[TWITCH] " <> show rawMsg
      hFlush logHandle
      case cookedMsg of
        Ping xs -> atomically $ writeQueue outgoingQueue (ircPong xs)
        Join _ channelId _ ->
          atomically $ modifyTVar channels $ S.insert channelId
        Part _ channelId _ ->
          atomically $ modifyTVar channels $ S.delete channelId
        Privmsg userInfo channelId message -> do
          roles <-
            maybe
              (return [])
              (getTwitchUserRoles dbConn)
              (userIdFromRawIrcMsg rawMsg)
          let badgeRoles = badgeRolesFromRawIrcMsg rawMsg
          let displayName = lookupEntryValue "display-name" $ _msgTags rawMsg
          logMessage
            dbConn
            (TwitchIrcChannel channelId)
            (userIdFromRawIrcMsg rawMsg)
            (idText $ userNick userInfo)
            displayName
            roles
            badgeRoles
            message
          addMarkovSentence dbConn message
          -- FIXME(#31): Link filtering is not disablable
          evalResult <-
            runExceptT $
            evalStateT (evalCommandPipe $ parseCommandPipe "!" "|" message) $
            EvalContext
              { evalContextVars =
                  M.fromList [("sender", idText (userNick userInfo))]
              , evalContextSqliteConnection = dbConn
              , evalContextSenderId = userIdFromRawIrcMsg rawMsg
              , evalContextSenderName = idText (userNick userInfo)
              , evalContextChannel = TwitchIrcChannel channelId
              , evalContextBadgeRoles = badgeRoles
              , evalContextRoles = roles
              , evalContextLogHandle = logHandle
              , evalContextTwitchEmotes =
                  do emotesTag <- lookupEntryValue "emotes" $ _msgTags rawMsg
                     if (not $ T.null emotesTag)
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
                ircPrivmsg (idText channelId) $ twitchCmdEscape commandResponse
              Left (EvalError userMsg) ->
                writeQueue outgoingQueue $
                ircPrivmsg (idText channelId) $ twitchCmdEscape userMsg
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
  botThread' dbConn botState
  where
    twitchCmdEscape :: T.Text -> T.Text
    twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

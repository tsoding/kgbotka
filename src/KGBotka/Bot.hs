{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module KGBotka.Bot
  ( botThread
  , BotState(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Eval
import Control.Monad.Trans.Except
import Control.Monad.Trans.Extra
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Array
import Data.Foldable
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
import KGBotka.Asciify
import KGBotka.Bttv
import KGBotka.Command
import KGBotka.Expr
import KGBotka.Ffz
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
import qualified Text.Regex.Base.RegexLike as Regex
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String
import Control.Exception
import Data.List

data EvalContext = EvalContext
  { evalContextVars :: M.Map T.Text T.Text
  , evalContextSqliteConnection :: Sqlite.Connection
  , evalContextSenderId :: TwitchUserId
  , evalContextSenderName :: T.Text
  -- TODO(#80): evalContextTwitchEmotes should be a list of some kind of emote type
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

failIfNotTrusted :: Eval ()
failIfNotTrusted = do
  roles <- evalContextRoles <$> getEval
  badgeRoles <- evalContextBadgeRoles <$> getEval
  when (null roles && null badgeRoles) $
    throwExceptEval $ EvalError "Only for trusted users"

evalExpr :: Expr -> Eval T.Text
evalExpr (TextExpr t) = return t
evalExpr (FunCallExpr "or" args) =
  fromMaybe "" . listToMaybe . dropWhile T.null <$> mapM evalExpr args
evalExpr (FunCallExpr "urlencode" args) =
  T.concat . map (T.pack . encodeURI . T.unpack) <$> mapM evalExpr args
  where
    encodeURI = escapeURIString (const False)
evalExpr (FunCallExpr "markov" _) = do
  dbConn <- evalContextSqliteConnection <$> getEval
  logHandle <- evalContextLogHandle <$> getEval
  events <- liftIO $ seqMarkovEvents Begin End dbConn
  liftIO $ hPutStrLn logHandle $ "[MARKOV] " <> show events
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
  badgeRoles <- evalContextBadgeRoles <$> getEval
  if TwitchBroadcaster `elem` badgeRoles
    then do
      dbConn <- evalContextSqliteConnection <$> getEval
      channel <- evalContextChannel <$> getEval
      fridayVideo <-
        liftExceptT $
        maybeToExceptT (EvalError "Video queue is empty") $
        nextVideo dbConn channel
      return $ fridayVideoAsMessage fridayVideo
    else throwExceptEval $ EvalError "Only for mr strimmer :)"
  where
    fridayVideoAsMessage :: FridayVideo -> T.Text
    fridayVideoAsMessage FridayVideo { fridayVideoSubText = subText
                                     , fridayVideoSubTime = subTime
                                     , fridayVideoAuthorTwitchName = authorTwitchName
                                     } =
      T.pack (show subTime) <> " <" <> authorTwitchName <> "> " <> subText
-- FIXME(#39): %friday does not inform how many times a video was suggested
evalExpr (FunCallExpr "friday" args) = do
  roles <- evalContextRoles <$> getEval
  badgeRoles <- evalContextBadgeRoles <$> getEval
  when (null roles && null badgeRoles) $
    throwExceptEval $ EvalError "You have to be trusted to submit Friday videos"
  submissionText <- T.concat <$> mapM evalExpr args
  case ytLinkId submissionText of
    Right _ -> do
      senderId <- evalContextSenderId <$> getEval
      dbConn <- evalContextSqliteConnection <$> getEval
      channel <- evalContextChannel <$> getEval
      senderName <- evalContextSenderName <$> getEval
      liftIO $ submitVideo dbConn submissionText channel senderId senderName
      return "Added your video to suggestions"
    Left Nothing ->
      throwExceptEval $ EvalError "Your suggestion should contain YouTube link"
    Left (Just failReason) -> do
      logHandle <- evalContextLogHandle <$> getEval
      liftIO $
        hPutStrLn logHandle $
        "An error occured while parsing YouTube link: " <> failReason
      throwExceptEval $
        EvalError
          "Something went wrong while parsing your subsmission. \
          \We are already looking into it. Kapp"
evalExpr (FunCallExpr "asciify" args) = do
  failIfNotTrusted
  emotes <- evalContextTwitchEmotes <$> getEval
  let twitchEmoteUrl =
        let makeTwitchEmoteUrl emoteName =
              "https://static-cdn.jtvnw.net/emoticons/v1/" <> emoteName <>
              "/3.0"
         in makeTwitchEmoteUrl <$> hoistMaybe emotes
  dbConn <- evalContextSqliteConnection <$> getEval
  channel <- evalContextChannel <$> getEval
  emoteNameArg <- T.concat <$> mapM evalExpr args
  let bttvEmoteUrl =
        bttvEmoteImageUrl <$>
        getBttvEmoteByName dbConn emoteNameArg (Just channel)
  let ffzEmoteUrl =
        ffzEmoteImageUrl <$>
        getFfzEmoteByName dbConn emoteNameArg (Just channel)
  emoteUrl <-
    liftExceptT $
    maybeToExceptT
      (EvalError "No emote found")
      (twitchEmoteUrl <|> bttvEmoteUrl <|> ffzEmoteUrl)
  manager <- evalContextManager <$> getEval
  image <- liftIO $ runExceptT $ asciifyUrl dbConn manager emoteUrl
  case image of
    Right image' -> return image'
    Left errorMessage -> do
      logHandle <- evalContextLogHandle <$> getEval
      senderName <- evalContextSenderName <$> getEval
      liftIO $ hPutStrLn logHandle errorMessage
      throwExceptEval $ EvalError ("@" <> senderName <> " Could not load emote")
evalExpr (FunCallExpr funame _) = do
  vars <- evalContextVars <$> getEval
  liftExceptT $
    maybeToExceptT (EvalError $ "Function `" <> funame <> "` does not exists") $
    hoistMaybe $ M.lookup funame vars

evalExprs :: [Expr] -> Eval T.Text
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
  , botStateChannels :: !(TVar (S.Set TwitchIrcChannel))
  , botStateSqliteFileName :: !FilePath
  , botStateLogHandle :: !Handle
  , botStateManager :: !HTTP.Manager
  }

type Eval = EvalT EvalContext EvalError IO

evalCommandCall :: CommandCall -> Eval T.Text
evalCommandCall (CommandCall name args) = do
  modifyEval $ evalContextVarsModify $ M.insert "1" args
  dbConn <- evalContextSqliteConnection <$> getEval
  command <- liftIO $ commandByName dbConn name
  senderName <- evalContextSenderName <$> getEval
  case command of
    Just Command {commandId = commandIdent, commandCode = code} -> do
      senderId <- evalContextSenderId <$> getEval
      cooledDown <- liftIO $ isCommandCooleddown dbConn senderId commandIdent
      unless cooledDown $
        throwExceptEval $
        EvalError $ "@" <> senderName <> " The command has not cooled down yet"
      liftIO $ logCommand dbConn senderId commandIdent args
      codeAst <-
        liftExceptT $
        withExceptT (EvalError . T.pack . show) $
        except (snd <$> runParser exprs code)
      evalExprs codeAst
    Nothing -> return ""

evalCommandPipe :: [CommandCall] -> Eval T.Text
evalCommandPipe =
  foldlM (\args -> evalCommandCall . ccArgsModify (`T.append` args)) ""

botThread :: BotState -> IO ()
botThread botState = do
  let databaseFileName = botStateSqliteFileName botState
  withConnectionAndPragmas databaseFileName $ \conn ->
    botThread' conn botState

processControlMsgs :: [RawIrcMsg] -> BotState -> IO ()
processControlMsgs messages botState = do
  let logHandle = botStateLogHandle botState
  let outgoingQueue = botStateOutgoingQueue botState
  let channels = botStateChannels botState
  for_ messages $ \msg -> do
    let cookedMsg = cookIrcMsg msg
    hPutStrLn logHandle $ "[TWITCH] " <> show msg
    hFlush logHandle
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

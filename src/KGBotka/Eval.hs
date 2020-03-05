{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Eval
  ( EvalContext(..)
  , evalCommandCall
  , evalCommandPipe
  , EvalError(..)
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Eval
import Control.Monad.Trans.Except
import Control.Monad.Trans.Extra
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import qualified Database.SQLite.Simple as Sqlite
import KGBotka.Asciify
import KGBotka.Bttv
import KGBotka.Command
import KGBotka.Config
import KGBotka.Expr
import KGBotka.Ffz
import KGBotka.Flip
import KGBotka.Friday
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Parser
import KGBotka.Queue
import KGBotka.Roles
import KGBotka.TwitchAPI
import qualified Network.HTTP.Client as HTTP
import Network.URI
import qualified Text.Regex.Base.RegexLike as Regex
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String

data EvalContext = EvalContext
  { ecVars :: M.Map T.Text T.Text
  , ecSqliteConnection :: Sqlite.Connection
  , ecSenderId :: TwitchUserId
  , ecSenderName :: T.Text
  -- TODO(#80): evalContextTwitchEmotes should be a list of some kind of emote type
  , ecTwitchEmotes :: Maybe T.Text
  , ecChannel :: TwitchIrcChannel
  , ecBadgeRoles :: [TwitchBadgeRole]
  , ecRoles :: [TwitchRole]
  , ecManager :: HTTP.Manager
  , ecLogQueue :: !(WriteQueue LogEntry)
  , ecConfigTwitch :: !ConfigTwitch
  }

newtype EvalError =
  EvalError T.Text
  deriving (Show)

type Eval = EvalT EvalContext EvalError IO

evalCommandCall :: CommandCall -> Eval T.Text
evalCommandCall (CommandCall name args) = do
  modifyEval $ ecVarsModify $ M.insert "1" args
  dbConn <- ecSqliteConnection <$> getEval
  command <- liftIO $ commandByName dbConn name
  senderName <- ecSenderName <$> getEval
  case command of
    Just Command {commandId = commandIdent, commandCode = code} -> do
      senderId <- ecSenderId <$> getEval
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

ecVarsModify ::
     (M.Map T.Text T.Text -> M.Map T.Text T.Text) -> EvalContext -> EvalContext
ecVarsModify f context =
  context {ecVars = f $ ecVars context}

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

failIfNotTrusted :: Eval ()
failIfNotTrusted = do
  roles <- ecRoles <$> getEval
  badgeRoles <- ecBadgeRoles <$> getEval
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
  dbConn <- ecSqliteConnection <$> getEval
  events <- liftIO $ seqMarkovEvents Begin End dbConn
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
  badgeRoles <- ecBadgeRoles <$> getEval
  if TwitchBroadcaster `elem` badgeRoles
    then do
      dbConn <- ecSqliteConnection <$> getEval
      channel <- ecChannel <$> getEval
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
  roles <- ecRoles <$> getEval
  badgeRoles <- ecBadgeRoles <$> getEval
  when (null roles && null badgeRoles) $
    throwExceptEval $ EvalError "You have to be trusted to submit Friday videos"
  submissionText <- T.concat <$> mapM evalExpr args
  case ytLinkId submissionText of
    Right _ -> do
      senderId <- ecSenderId <$> getEval
      dbConn <- ecSqliteConnection <$> getEval
      channel <- ecChannel <$> getEval
      senderName <- ecSenderName <$> getEval
      liftIO $ submitVideo dbConn submissionText channel senderId senderName
      return "Added your video to suggestions"
    Left Nothing ->
      throwExceptEval $ EvalError "Your suggestion should contain YouTube link"
    Left (Just failReason) -> do
      logQueue <- ecLogQueue <$> getEval
      liftIO $
        atomically $
        writeQueue logQueue $
        LogEntry "YOUTUBE" $
        "An error occured while parsing YouTube link: " <> T.pack failReason
      throwExceptEval $
        EvalError
          "Something went wrong while parsing your subsmission. \
          \We are already looking into it. Kapp"
evalExpr (FunCallExpr "asciify" args) = do
  failIfNotTrusted
  emotes <- ecTwitchEmotes <$> getEval
  let twitchEmoteUrl =
        let makeTwitchEmoteUrl emoteName =
              "https://static-cdn.jtvnw.net/emoticons/v1/" <> emoteName <>
              "/3.0"
         in makeTwitchEmoteUrl <$> hoistMaybe emotes
  dbConn <- ecSqliteConnection <$> getEval
  channel <- ecChannel <$> getEval
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
  manager <- ecManager <$> getEval
  image <- liftIO $ runExceptT $ asciifyUrl dbConn manager emoteUrl
  case image of
    Right image' -> return image'
    Left errorMessage -> do
      logQueue <- ecLogQueue <$> getEval
      senderName <- ecSenderName <$> getEval
      liftIO $
        atomically $
        writeQueue logQueue $ LogEntry "ASCIIFY" $ T.pack errorMessage
      throwExceptEval $ EvalError ("@" <> senderName <> " Could not load emote")
evalExpr (FunCallExpr "help" args) = do
  name <- T.concat <$> mapM evalExpr args
  dbConn <- ecSqliteConnection <$> getEval
  maybeCommand <- liftIO $ commandByName dbConn name
  case maybeCommand of
    Just Command {commandCode = code} ->
      return $ "Command `" <> name <> "` defined as `" <> code <> "`"
    Nothing -> return $ "Command `" <> name <> " does not exist"
evalExpr (FunCallExpr "uptime" _) = do
  manager <- ecManager <$> getEval
  config <- ecConfigTwitch <$> getEval
  channel <- ecChannel <$> getEval
  logQueue <- ecLogQueue <$> getEval
  stream <-
    liftIO $
    getStreamByLogin
      manager
      (configTwitchClientId config)
      (twitchIrcChannelName channel)
  case stream of
    Right (Just TwitchStream {tsStartedAt = startedAt}) -> do
      now <- liftIO getCurrentTime
      return $ humanReadableDiffTime $ diffUTCTime now startedAt
    Right Nothing -> do
      liftIO $
        atomically $
        writeQueue logQueue $
        LogEntry "TWITCHAPI" $
        "No streams for " <> twitchIrcChannelText channel <> " were found"
      return ""
    Left errorMessage -> do
      liftIO $
        atomically $
        writeQueue logQueue $ LogEntry "TWITCHAPI" $ T.pack errorMessage
      return ""
evalExpr (FunCallExpr funame _) = do
  vars <- ecVars <$> getEval
  liftExceptT $
    maybeToExceptT (EvalError $ "Function `" <> funame <> "` does not exists") $
    hoistMaybe $ M.lookup funame vars

evalExprs :: [Expr] -> Eval T.Text
evalExprs exprs' = T.concat <$> mapM evalExpr exprs'

humanReadableDiffTime :: NominalDiffTime -> T.Text
humanReadableDiffTime t
  | t < 1.0 = "< 1 second"
  | otherwise =
    T.unwords $
    map (\(name, amount) -> T.pack (show amount) <> " " <> name) $
    filter ((> 0) . snd) components
  where
    s :: Int
    s = round t
    components :: [(T.Text, Int)]
    components =
      [ ("days" :: T.Text, s `div` secondsInDay)
      , ("hours", (s `mod` secondsInDay) `div` secondsInHour)
      , ( "minutes"
        , ((s `mod` secondsInDay) `mod` secondsInHour) `div` secondsInMinute)
      , ( "seconds"
        , ((s `mod` secondsInDay) `mod` secondsInHour) `mod` secondsInMinute)
      ]
    secondsInDay = 24 * secondsInHour
    secondsInHour = 60 * secondsInMinute
    secondsInMinute = 60

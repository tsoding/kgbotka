{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module KGBotka.Eval
  ( EvalContext(..)
  , EvalPlatformContext(..)
  , EvalTwitchContext(..)
  , EvalDiscordContext(..)
  , evalCommandCall
  , evalCommandPipe
  , EvalError(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Eval
import Control.Monad.Trans.Except
import Control.Monad.Trans.Extra
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Bifunctor (first)
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Data.Word
import qualified Database.SQLite.Simple as Sqlite
import Discord.Types
import KGBotka.Asciify
import KGBotka.Bttv
import KGBotka.Calc
import KGBotka.Command
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
import System.Random
import Text.Printf
import qualified Text.Regex.Base.RegexLike as Regex
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String

data EvalTwitchContext = EvalTwitchContext
  { etcSenderId :: !TwitchUserId
  , etcSenderName :: !T.Text
  -- TODO(#80): evalContextTwitchEmotes should be a list of some kind of emote type
  , etcTwitchEmotes :: !(Maybe T.Text)
  , etcChannel :: !TwitchIrcChannel
  , etcBadgeRoles :: ![TwitchBadgeRole]
  , etcRoles :: ![TwitchRole]
  , etcClientId :: !T.Text
  }

data EvalDiscordContext = EvalDiscordContext
  { edcAuthor :: !User
  , edcGuild :: !(Maybe Guild)
  , edcRoles :: ![Snowflake]
  }

data EvalPlatformContext
  = Etc EvalTwitchContext
  | Edc EvalDiscordContext

data EvalContext = EvalContext
  { ecVars :: !(M.Map T.Text T.Text)
  , ecSqliteConnection :: !Sqlite.Connection
  , ecManager :: !HTTP.Manager
  , ecLogQueue :: !(WriteQueue LogEntry)
  , ecFridayGistUpdateRequired :: !(MVar ())
  , ecPlatformContext :: !EvalPlatformContext
  }

instance ProvidesLogging EvalContext where
  logQueue = ecLogQueue

logEntryEval :: LogEntry -> Eval ()
logEntryEval entry = do
  context <- getEval
  liftIO $ logEntry context entry

newtype EvalError =
  EvalError T.Text
  deriving (Show)

type Eval = EvalT EvalContext EvalError IO

evalCommandCall :: CommandCall -> Eval T.Text
evalCommandCall (CommandCall name args) = do
  modifyEval $ ecVarsModify $ M.insert "1" args
  dbConn <- ecSqliteConnection <$> getEval
  command <- liftIO $ commandByName dbConn name
  case command of
    Just Command { commandId = commandIdent
                 , commandCode = code
                 , commandTimes = times
                 } -> do
      modifyEval $ ecVarsModify $ M.insert "times" $ T.pack $ show times
      platformContext <- ecPlatformContext <$> getEval
      case platformContext of
        Etc etc -> do
          let senderId = etcSenderId etc
          cooledDown <-
            liftIO $
            isCommandCooleddown dbConn Nothing (Just senderId) commandIdent
          unless cooledDown $
            throwExceptEval $
            EvalError $
            "@" <> etcSenderName etc <> " The command has not cooled down yet"
          liftIO $ logCommand dbConn Nothing (Just senderId) commandIdent args
        Edc edc -> do
          let senderId = DiscordUserId $ userId $ edcAuthor edc
          cooledDown <-
            liftIO $
            isCommandCooleddown dbConn (Just senderId) Nothing commandIdent
          unless cooledDown $
            throwExceptEval $
            EvalError $
            T.pack $
            printf
              "<@!%d> The command has not cooled down yet"
              ((fromIntegral $ userId $ edcAuthor edc) :: Word64)
          liftIO $ logCommand dbConn (Just senderId) Nothing commandIdent args
      codeAst <-
        liftExceptT $
        withExceptT (EvalError . T.pack . show) $
        except (snd <$> runParser exprs code)
      responseText <- evalExprs codeAst
      liftIO $ bumpCommandTimes dbConn commandIdent
      return responseText
    Nothing -> return ""

evalCommandPipe :: [CommandCall] -> Eval T.Text
evalCommandPipe =
  foldlM (\args -> evalCommandCall . ccArgsModify (`T.append` args)) ""

ecVarsModify ::
     (M.Map T.Text T.Text -> M.Map T.Text T.Text) -> EvalContext -> EvalContext
ecVarsModify f context = context {ecVars = f $ ecVars context}

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
  platformContext <- ecPlatformContext <$> getEval
  case platformContext of
    Etc etc ->
      let roles = etcRoles etc
          badgeRoles = etcBadgeRoles etc
       in when (null roles && null badgeRoles) $
          throwExceptEval $ EvalError "Only for trusted users"
    Edc edc ->
      when (null $ edcRoles edc) $
      throwExceptEval $ EvalError "Only for trusted users"

failIfNotAuthority :: Eval ()
failIfNotAuthority = do
  platformContext <- ecPlatformContext <$> getEval
  case platformContext of
    Etc EvalTwitchContext {etcBadgeRoles = badgeRoles}
      | TwitchBroadcaster `elem` badgeRoles -> return ()
    Edc EvalDiscordContext { edcAuthor = User {userId = authorId}
                           , edcGuild = Just Guild {guildOwnerId = ownerId}
                           }
      | authorId == ownerId -> return ()
    _ -> throwExceptEval $ EvalError "Only for mr strimmer :)"

requireFridayGistUpdate :: Eval ()
requireFridayGistUpdate = do
  fridayGistUpdateRequired <- ecFridayGistUpdateRequired <$> getEval
  void $ liftIO $ tryPutMVar fridayGistUpdateRequired ()

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
  liftIO $ genMarkovSentence dbConn
evalExpr (FunCallExpr "flip" args) =
  T.concat . map flipText <$> mapM evalExpr args
-- FIXME(#38): %nextvideo does not inform how many times a video was suggested
evalExpr (FunCallExpr "nextvideo" _) = do
  failIfNotAuthority
  dbConn <- ecSqliteConnection <$> getEval
  fridayVideo <-
    liftExceptT $
    maybeToExceptT (EvalError "Video queue is empty") $ nextVideo dbConn
  requireFridayGistUpdate
  return $ fridayVideoAsMessage fridayVideo
-- FIXME(#39): %friday does not inform how many times a video was suggested
evalExpr (FunCallExpr "friday" args) = do
  failIfNotTrusted
  submissionText <- T.concat <$> mapM evalExpr args
  case ytLinkId submissionText of
    Right _ -> do
      platformContext <- ecPlatformContext <$> getEval
      dbConn <- ecSqliteConnection <$> getEval
      let (authorDisplayName, authorId) =
            case platformContext of
              Etc etc ->
                let TwitchUserId senderId = etcSenderId etc
                 in (etcSenderName etc, senderId)
              Edc edc ->
                ( userName $ edcAuthor edc
                , T.pack $ show $ userId $ edcAuthor edc)
      liftIO $
        submitVideo dbConn submissionText (AuthorId authorId) authorDisplayName
      requireFridayGistUpdate
      return "Added your video to suggestions"
    Left Nothing ->
      throwExceptEval $ EvalError "Your suggestion should contain YouTube link"
    Left (Just failReason) -> do
      logEntryEval $
        LogEntry "YOUTUBE" $
        "An error occured while parsing YouTube link: " <> T.pack failReason
      throwExceptEval $
        EvalError
          "Something went wrong while parsing your subsmission. \
          \We are already looking into it. Kapp"
evalExpr (FunCallExpr "asciify" args) = do
  failIfNotTrusted
  platformContext <- ecPlatformContext <$> getEval
  dbConn <- ecSqliteConnection <$> getEval
  emoteNameArg <- T.concat <$> mapM evalExpr args
  manager <- ecManager <$> getEval
  image <-
    case platformContext of
      Etc etc -> do
        let twitchEmoteUrl =
              let emotes = etcTwitchEmotes etc
                  makeTwitchEmoteUrl emoteName =
                    "https://static-cdn.jtvnw.net/emoticons/v1/" <> emoteName <>
                    "/3.0"
               in makeTwitchEmoteUrl <$> hoistMaybe emotes
        let channel = etcChannel etc
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
        liftIO $ runExceptT $ asciifyUrl dbConn manager emoteUrl
      Edc _ -> do
        regex <-
          liftExceptT $
          withExceptT (EvalError . T.pack) $
          except $ compile defaultCompOpt defaultExecOpt "<\\:.+\\:([0-9]+)>"
        case execute regex (T.unpack emoteNameArg) of
          Right (Just matches) ->
            case map (T.pack . flip Regex.extract (T.unpack emoteNameArg)) $
                 elems matches of
              [_, discordEmoteId] ->
                liftIO $
                runExceptT
                  (T.unlines . T.splitOn " " <$>
                   asciifyUrl
                     dbConn
                     manager
                     ("https://cdn.discordapp.com/emojis/" <> discordEmoteId <>
                      ".png"))
              _ -> throwExceptEval $ EvalError "No emote found"
          _ -> throwExceptEval $ EvalError "No emote found"
  case image of
    Right image' -> return image'
    Left errorMessage -> do
      logEntryEval $ LogEntry "ASCIIFY" $ T.pack errorMessage
      return ""
evalExpr (FunCallExpr "tsify" args) = do
  text <- T.concat <$> mapM evalExpr args
  return $
    T.concatMap
      (\case
         'c' -> "ts"
         'C' -> "Ts"
         x -> T.pack [x])
      text
evalExpr (FunCallExpr "help" args) = do
  name <- T.concat <$> mapM evalExpr args
  dbConn <- ecSqliteConnection <$> getEval
  maybeCommand <- liftIO $ commandByName dbConn name
  case maybeCommand of
    Just Command {commandCode = code} ->
      return $ "Command `" <> name <> "` defined as `" <> code <> "`"
    Nothing -> return $ "Command `" <> name <> " does not exist"
evalExpr (FunCallExpr "uptime" _) = do
  platformContext <- ecPlatformContext <$> getEval
  case platformContext of
    Etc etc -> do
      manager <- ecManager <$> getEval
      let clientId = etcClientId etc
      let channel = etcChannel etc
      stream <-
        liftIO $
        getStreamByLogin manager clientId (twitchIrcChannelName channel)
      case stream of
        Right (Just TwitchStream {tsStartedAt = startedAt}) -> do
          now <- liftIO getCurrentTime
          return $ humanReadableDiffTime $ diffUTCTime now startedAt
        Right Nothing -> do
          logEntryEval $
            LogEntry "TWITCHAPI" $
            "No streams for " <> twitchIrcChannelText channel <> " were found"
          return ""
        Left errorMessage -> do
          logEntryEval $ LogEntry "TWITCHAPI" $ T.pack errorMessage
          return ""
    Edc _ -> throwExceptEval $ EvalError "Uptime doesn't work in Discord"
evalExpr (FunCallExpr "eval" args) = do
  failIfNotAuthority
  code <- T.concat <$> mapM evalExpr args
  codeAst <-
    liftExceptT $
    withExceptT (EvalError . T.pack . show) $
    except (snd <$> runParser exprs code)
  evalExprs codeAst
evalExpr (FunCallExpr "calc" args) = do
  mathsExpression <- T.concat <$> mapM evalExpr args
  (rest, parsedExpression) <-
    exceptEval $
    first parserStopToEvalError $ runParser parseLine mathsExpression
  case T.unpack rest of
    [] -> do
      value <- evalCalcExpression parsedExpression
      return $ T.pack $ show value
    _ -> throwExceptEval $ EvalError "Calc: Incomplete parse PepeHands"
  where
    parserStopToEvalError EOF = EvalError "Calc: Unexpected EOF"
    parserStopToEvalError (SyntaxError msg) =
      EvalError $ "Syntax error: " <> msg
evalExpr (FunCallExpr "roles" _) = do
  platformContext <- ecPlatformContext <$> getEval
  case platformContext of
    Etc etc ->
      return $
      T.pack $
      printf
        "@%s Your badge roles: %s. Your custom roles: %s"
        (etcSenderName etc)
        (show $ etcBadgeRoles etc)
        (show $ etcRoles etc)
    Edc edc ->
      return $
      T.pack $
      printf
        "<@!%d> Your roles: %s."
        ((fromIntegral $ userId $ edcAuthor edc) :: Word64)
        (show $ edcRoles edc)
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

evalCalcExpression :: CalcExpression -> Eval Double
evalCalcExpression (BinaryExpression op left right) = do
  left' <- evalCalcExpression left
  right' <- evalCalcExpression right
  return $
    (case op of
       Add -> (+)
       Sub -> (-)
       Mul -> (*)
       Div -> (/)
       Mod ->
         \a b ->
           fromIntegral $
           mod (toInteger (floor a :: Integer)) (toInteger (floor b :: Integer))
       Pow -> (**))
      left'
      right'
evalCalcExpression (NegativeExpression body) =
  (* (-1.0)) <$> evalCalcExpression body
evalCalcExpression (ValueExpression val) = return val
evalCalcExpression (FunctionApplication functionName args) =
    case M.lookup functionName functionLookupTable of
      Just f -> mapM evalCalcExpression args >>= f
      Nothing -> throwExceptEval $ EvalError $ "undefined is not a function FeelsDankMan"

functionLookupTable :: M.Map T.Text ([Double] -> Eval Double)
functionLookupTable =
  M.fromList
    [ ( "sin"
      , \case
          [x] -> return $ sin x
          _ -> throwExceptEval $ EvalError "sin expects one argument")
    , ( "cos"
      , \case
          [x] -> return $ cos x
          _ -> throwExceptEval $ EvalError "cos expects one argument")
    , ( "tan"
      , \case
          [x] -> return $ tan x
          _ -> throwExceptEval $ EvalError "tan expects one argument")
    , ( "arcsin"
      , \case
          [x] -> return $ asin x
          _ -> throwExceptEval $ EvalError "arcsin expects one argument")
    , ( "arccos"
      , \case
          [x] -> return $ acos x
          _ -> throwExceptEval $ EvalError "arccos expects one argument")
    , ( "arctan"
      , \case
          [x] -> return $ atan x
          _ -> throwExceptEval $ EvalError "arctan expects one argument")
    , ( "exp"
      , \case
          [x] -> return $ exp x
          _ -> throwExceptEval $ EvalError "exp expects one argument")
    , ( "nthroot"
      , \case
          [n, x] -> return $ n ** recip x
          _ -> throwExceptEval $ EvalError "nthroot expects two arguments (radix and radicand)")
    , ( "sqrt"
      , \case
          [x] -> return $ x ** 0.5
          _ -> throwExceptEval $ EvalError "sqrt expects one argument")
    , ( "random"
      , \case
          [] -> lift randomIO
          _ -> throwExceptEval $ EvalError "random takes no arguments")
    ]

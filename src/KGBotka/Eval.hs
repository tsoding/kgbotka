{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Eval
  ( EvalContext(..)
  , EvalPlatformContext(..)
  , EvalTwitchContext(..)
  , EvalDiscordContext(..)
  , EvalReplContext(..)
  , evalCommandCall
  , evalCommandPipe
  , evalExpr
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
import Data.Time
import Data.Word
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.QQ
import Discord.Types
import KGBotka.Asciify
import KGBotka.Bttv
import KGBotka.Calc
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
import KGBotka.Xkcd
import qualified Network.HTTP.Client as HTTP
import Network.URI
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
  , etcConfigTwitch :: !ConfigTwitch
  }

data EvalDiscordContext = EvalDiscordContext
  { edcAuthor :: !User
  , edcGuild :: !(Maybe Guild)
  , edcRoles :: ![Snowflake]
  }

data EvalReplContext =
  EvalReplContext

data EvalPlatformContext
  = Etc EvalTwitchContext
  | Edc EvalDiscordContext
  | Erc EvalReplContext

data EvalContext = EvalContext
  { ecVars :: !(M.Map T.Text T.Text)
  , ecSqliteConnection :: !Sqlite.Connection
  , ecManager :: !HTTP.Manager
  , ecLogQueue :: !(WriteQueue LogEntry)
  , ecFridayGistUpdateRequired :: !(MVar ())
  , ecPlatformContext :: !EvalPlatformContext
  }

instance ProvidesLogging EvalContext where
  logEntry ec = logEntry $ ecLogQueue ec

logEntryEval :: LogEntry -> Eval ()
logEntryEval entry = do
  context <- getEval
  liftIO $ logEntry context entry

newtype EvalError =
  EvalError T.Text
  deriving (Show)

type Eval = EvalT EvalContext EvalError IO

senderMentionOfContext :: EvalPlatformContext -> Maybe T.Text
senderMentionOfContext (Etc EvalTwitchContext {etcSenderName = name}) =
  Just name
senderMentionOfContext (Edc EvalDiscordContext {edcAuthor = author}) =
  Just $ T.pack $ printf "<@!%d>" ((fromIntegral $ userId author) :: Word64)
senderMentionOfContext (Erc EvalReplContext) = Nothing

channelNameOfContext :: EvalPlatformContext -> T.Text
channelNameOfContext (Etc EvalTwitchContext {etcChannel = channel}) =
  twitchIrcChannelName channel
channelNameOfContext _ = ""

evalCommandCall :: CommandCall -> Eval T.Text
evalCommandCall (CommandCall name args) = do
  dbConn <- ecSqliteConnection <$> getEval
  command <- liftIO $ commandByName dbConn name
  case command of
    Just Command { commandId = commandIdent
                 , commandCode = code
                 , commandTimes = times
                 } -> do
      day <- liftIO $ utctDay <$> getCurrentTime
      let (yearNum, monthNum, dayNum) = toGregorian day
      platformContext <- ecPlatformContext <$> getEval
      -- TODO(#211): make evalCommandCall parse command call input according to command's args regex
      modifyEval $ ecVarsModify $ M.insert "1" args
      modifyEval $ ecVarsModify $ M.insert "times" $ T.pack $ show times
      case senderMentionOfContext platformContext of
        Just sender -> modifyEval $ ecVarsModify $ M.insert "sender" sender
        Nothing -> return ()
      modifyEval $ ecVarsModify $ M.insert "year" $ T.pack $ show yearNum
      modifyEval $ ecVarsModify $ M.insert "month" $ T.pack $ show monthNum
      modifyEval $ ecVarsModify $ M.insert "day" $ T.pack $ show dayNum
      modifyEval $ ecVarsModify $ M.insert "date" $ T.pack $ showGregorian day
      -- TODO(#174): %tchannel is incosistent with the general variable behaviour
      --   Usually when variable is not available it throws an error. But %tchannel doesn't!
      --   It's simply empty on Discord. This kind of inconsistency is not acceptable.
      modifyEval $
        ecVarsModify $
        M.insert "tchannel" $ channelNameOfContext platformContext
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
        Erc _ ->
          throwExceptEval $
          EvalError "Chat commands are not supported in the REPL context"
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
    Erc _ -> return ()

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

wordsPerMinuteOnTwitch :: Sqlite.Connection -> T.Text -> IO Int
wordsPerMinuteOnTwitch dbConn input = do
  let term = fromMaybe "" $ listToMaybe $ textAsTerms input
  messages <-
    map Sqlite.fromOnly <$>
    Sqlite.queryNamed
      dbConn
      [sql| select message from TwitchLog
            where messageTime > datetime('now', '-1 minute') |]
      []
  let n = length $ filter (== T.toUpper term) $ concatMap textAsTerms messages
  return n

wordsPerMinuteOnDiscord :: Sqlite.Connection -> T.Text -> IO Int
wordsPerMinuteOnDiscord dbConn input = do
  let term = fromMaybe "" $ listToMaybe $ textAsTerms input
  messages <-
    map Sqlite.fromOnly <$>
    Sqlite.queryNamed
      dbConn
      [sql| select message from DiscordLog
            where messageTime > datetime('now', '-1 minute') |]
      []
  let n = length $ filter (== T.toUpper term) $ concatMap textAsTerms messages
  return n

evalExpr :: Expr -> Eval T.Text
evalExpr (TextExpr t) = return t
evalExpr (FunCallExpr "or" args) =
  fromMaybe "" . listToMaybe . dropWhile T.null <$> mapM evalExpr args
evalExpr (FunCallExpr "urlencode" args) =
  T.concat . map (T.pack . encodeURI . T.unpack) <$> mapM evalExpr args
  where
    encodeURI = escapeURIString (const False)
evalExpr (FunCallExpr "wpm" args) = do
  platformContext <- ecPlatformContext <$> getEval
  word <- listToMaybe <$> mapM evalExpr args
  dbConn <- ecSqliteConnection <$> getEval
  case platformContext of
    Etc _ ->
      case word of
        Just word' -> do
          n <- liftIO $ wordsPerMinuteOnTwitch dbConn word'
          return $ T.pack $ printf "%d %s per minute" n word'
        Nothing -> return ""
    Edc _ ->
      case word of
        Just word' -> do
          n <- liftIO $ wordsPerMinuteOnDiscord dbConn word'
          return $ T.pack $ printf "%d %s per minute" n word'
        Nothing -> return ""
    -- TODO(#256): Add %wpm support for REPL evaluation context
    Erc _ -> throwExceptEval $ EvalError "%wpm does not work in REPL yet Kapp"
evalExpr (FunCallExpr "markov" args) = do
  prefix <- fmap T.words . listToMaybe <$> mapM evalExpr args
  dbConn <- ecSqliteConnection <$> getEval
  (T.unwords (initSafe (fromMaybe [] prefix) <> [""]) <>) <$>
    liftIO (genMarkovSentence dbConn (prefix >>= lastMaybe))
  where
    initSafe :: [a] -> [a]
    initSafe [] = []
    initSafe xs = init xs
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe [x] = Just x
    lastMaybe (_:xs) = lastMaybe xs
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
              Erc _ -> ("Admin", "69")
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
      -- TODO: Add %asciify support for REPL evaluation context
      Erc _ ->
        throwExceptEval $
        EvalError "%asciify is not support in REPL evaluation context yet"
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
  -- TODO(#237): %xkcd function does not search by several terms
evalExpr (FunCallExpr "xkcd" args) = do
  probablyTerm <- listToMaybe <$> mapM evalExpr args
  dbConn <- ecSqliteConnection <$> getEval
  case probablyTerm of
    Just term -> do
      probablyXkcd <- liftIO $ searchXkcdInDbByTerm dbConn $ textAsTerms term
      case probablyXkcd of
        Just Xkcd {xkcdNum = num} ->
          return $ T.pack $ printf "https://xkcd.com/%d/" num
        Nothing -> return "No xkcd with such term was found"
    Nothing -> throwExceptEval $ EvalError "No term was provided"
evalExpr (FunCallExpr "uptime" _) = do
  platformContext <- ecPlatformContext <$> getEval
  case platformContext of
    Etc etc -> do
      manager <- ecManager <$> getEval
      let channel = etcChannel etc
      stream <-
        liftIO $
        getStreamByLogin
          manager
          (etcConfigTwitch etc)
          (twitchIrcChannelName channel)
      case stream of
        Right (Just TwitchStream {tsStartedAt = startedAt}) -> do
          now <- liftIO getCurrentTime
          return $ humanReadableDiffTime $ diffUTCTime now startedAt
        Right Nothing -> return "Not even streaming LULW"
        Left errorMessage -> do
          logEntryEval $ LogEntry "TWITCHAPI" $ T.pack $ show errorMessage
          return ""
    Edc _ -> throwExceptEval $ EvalError "Uptime doesn't work in Discord"
    Erc _ -> throwExceptEval $ EvalError "Uptime doesn't work in REPL"
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
  if T.null rest
    then do
      calcResult <- lift $ runExceptT $ evalCalcExpression parsedExpression
      calcResultToEval calcResult
    else throwExceptEval $ EvalError "Calc: Incomplete parse PepeHands"
  where
    parserStopToEvalError EOF = EvalError "Calc: Unexpected EOF"
    parserStopToEvalError (SyntaxError msg) =
      EvalError $ "Syntax error: " <> msg
    -- TODO(#179): There might be a better way to do the job of calcResultToEval
    -- instead of unwrapping the ExceptT
    calcResultToEval :: Either CalcEvalError Double -> Eval T.Text
    calcResultToEval calcResult =
      case calcResult of
        Left (CalcEvalError e) ->
          throwExceptEval $ EvalError ("Evaluation error: " <> e)
        Right e -> return $ T.pack $ show e
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
    Erc _ ->
      return
        "You are in the REPL. You don't need any roles. You can do whatever you want!"
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

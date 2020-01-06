{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Bot
  ( botThread
  , BotState(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Either
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
import KGBotka.Parser
import KGBotka.Queue
import KGBotka.Repl
import KGBotka.Roles
import Network.URI
import System.IO
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String
import KGBotka.Friday
import Data.Array
import qualified Text.Regex.Base.RegexLike as Regex
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

data EvalContext = EvalContext
  { evalContextVars :: M.Map T.Text T.Text
  , evalContextSqliteConnection :: Sqlite.Connection
  , evalContextSenderId :: Maybe TwitchUserId
  , evalContextBadgeRoles :: [TwitchBadgeRole]
  , evalContextRoles :: [TwitchRole]
  }

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

data EvalError = EvalError
  { evalErrorUserMessage :: T.Text
  , evalErrorLogMessage :: T.Text
  } deriving (Show)

evalExpr :: EvalContext -> Expr -> ExceptT EvalError IO T.Text
evalExpr _ (TextExpr t) = return t
evalExpr context (FunCallExpr "or" args) =
  fromMaybe "" . listToMaybe . dropWhile T.null <$> mapM (evalExpr context) args
evalExpr context (FunCallExpr "urlencode" args) =
  T.concat . map (T.pack . encodeURI . T.unpack) <$>
  mapM (evalExpr context) args
  where
    encodeURI = escapeURIString (const False)
evalExpr context (FunCallExpr "flip" args) =
  T.concat . map flipText <$> mapM (evalExpr context) args
-- FIXME(#17): there is no !nextvideo command
-- FIXME(#18): Friday video list is not published on gist
evalExpr EvalContext {evalContextRoles = [], evalContextBadgeRoles = []} (FunCallExpr "friday" _) =
  return "You have to be trusted to submit Friday videos"
evalExpr context (FunCallExpr "friday" args) = do
  submissionText <- T.concat <$> mapM (evalExpr context) args
  case ytLinkId submissionText of
    Right _ ->
      case evalContextSenderId context of
        Just senderId -> do
          lift $
            submitVideo
              (evalContextSqliteConnection context)
              submissionText
              senderId
          return "Added your video to suggestions"
        Nothing -> return "Only humans can submit friday videos"
    Left Nothing -> return "Your suggestion should contain YouTube link"
    Left (Just failReason) ->
      throwE $
        EvalError
          { evalErrorUserMessage =
              "Something went wrong while parsing your subsmission. \
              \We are already looking into it. Kapp"
          , evalErrorLogMessage =
              T.pack $
              "An error occured while parsing YouTube link: " <> failReason
          }
evalExpr context (FunCallExpr funame _) =
  return $ fromMaybe "" $ M.lookup funame (evalContextVars context)

evalExprs :: EvalContext -> [Expr] -> ExceptT EvalError IO T.Text
evalExprs context = fmap T.concat . mapM (evalExpr context)

textContainsLink :: T.Text -> Bool
textContainsLink t =
  isRight $ do
    regex <-
      compile
        defaultCompOpt
        defaultExecOpt
        "[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&\\/\\/=]*)"
    match <- execute regex $ T.unpack t
    case match of
      Just x -> Right x
      Nothing -> Left "No match found"

data TwitchBadgeRole
  = TwitchSub
  | TwitchVip
  | TwitchBroadcaster
  | TwitchMod
  deriving (Show)

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
  , botStateSqliteConnection :: !Sqlite.Connection
  , botStateLogHandle :: !Handle
  }

botThread :: BotState -> IO ()
botThread state@BotState { botStateIncomingQueue = incomingQueue
                         , botStateOutgoingQueue = outgoingQueue
                         , botStateReplQueue = replQueue
                         , botStateChannels = channels
                         , botStateSqliteConnection = dbConn
                         , botStateLogHandle = logHandle
                         } = do
  threadDelay 10000 -- to prevent busy looping
  maybeRawMsg <- atomically $ tryReadQueue incomingQueue
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
        -- FIXME: Link filtering is not disablable
        case (roles, badgeRoles) of
          ([], [])
            | textContainsLink message ->
              atomically $
              writeQueue outgoingQueue $
              ircPrivmsg
                (idText channelId)
                ("/timeout " <> idText (userNick userInfo) <> " 1")
          _ ->
            case parseCommandCall "!" message of
              Just (CommandCall name args) -> do
                command <- commandByName dbConn name
                case command of
                  Just (Command _ code) ->
                    let codeAst = snd <$> runParser exprs code
                     in case codeAst of
                          Right codeAst' -> do
                            evalResult <-
                              runExceptT $
                              evalExprs
                                (EvalContext
                                   (M.fromList
                                      [ ("1", args)
                                      , ("sender", idText (userNick userInfo))
                                      ])
                                   dbConn
                                   (userIdFromRawIrcMsg rawMsg)
                                   badgeRoles
                                   roles)
                                codeAst'
                            case evalResult of
                              Right commandResponse ->
                                atomically $
                                writeQueue outgoingQueue $
                                ircPrivmsg (idText channelId) $
                                twitchCmdEscape commandResponse
                              Left (EvalError userMsg logMsg) -> do
                                hPutStrLn logHandle $
                                  "[ERROR] " <> T.unpack logMsg
                                hFlush logHandle
                                atomically $
                                  writeQueue outgoingQueue $
                                  ircPrivmsg (idText channelId) $
                                  twitchCmdEscape userMsg
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
  botThread state
  where
    twitchCmdEscape :: T.Text -> T.Text
    twitchCmdEscape = T.dropWhile (`elem` ['/', '.']) . T.strip

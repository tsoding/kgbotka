{-# LANGUAGE OverloadedStrings #-}

module KGBotka.DiscordThread
  ( DiscordThreadParams(..)
  , discordThread
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Eval
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import qualified Database.SQLite.Simple as Sqlite
import Discord
import Discord.Requests
import qualified Discord.Requests as R
import Discord.Types
import KGBotka.Command
import KGBotka.Config
import KGBotka.DiscordLog
import KGBotka.Eval
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Queue
import qualified Network.HTTP.Client as HTTP
import Text.Printf

data DiscordThreadParams = DiscordThreadParams
  { dtpConfig :: !(Maybe ConfigDiscord)
  , dtpLogQueue :: !(WriteQueue LogEntry)
  , dtpSqliteConnection :: !(MVar Sqlite.Connection)
  , dtpManager :: !HTTP.Manager
  }

data DiscordThreadState = DiscordThreadState
  { dtsLogQueue :: !(WriteQueue LogEntry)
  , dtsSqliteConnection :: !(MVar Sqlite.Connection)
  , dtsManager :: !HTTP.Manager
  , dtsCurrentUser :: !(MVar User)
  }

instance ProvidesLogging DiscordThreadState where
  logQueue = dtsLogQueue

instance ProvidesLogging DiscordThreadParams where
  logQueue = dtpLogQueue

discordThread :: DiscordThreadParams -> IO ()
discordThread dtp =
  case dtpConfig dtp of
    Just config -> do
      currentUser <- newEmptyMVar
      let dts =
            DiscordThreadState
              { dtsLogQueue = dtpLogQueue dtp
              , dtsSqliteConnection = dtpSqliteConnection dtp
              , dtsManager = dtpManager dtp
              , dtsCurrentUser = currentUser
              }
      userFacingError <-
        runDiscord $
        def
          { discordToken = configDiscordToken config
          , discordOnEvent = eventHandler dts
          , discordOnStart = discordThreadOnStart dts
          }
      logEntry dtp $ LogEntry "DISCORD" userFacingError
    Nothing ->
      logEntry dtp $
      LogEntry "DISCORD" "[ERROR] Discord configuration not found"

discordThreadOnStart :: DiscordThreadState -> DiscordHandle -> IO ()
discordThreadOnStart dts dis = do
  response <- restCall dis GetCurrentUser
  case response of
    Right user -> putMVar (dtsCurrentUser dts) user
    Left err -> logEntry dts $ LogEntry "DISCORD" $ T.pack $ show err

-- TODO(#96): Discord messages are not logged
-- TODO(#97): Discord messages do not contribute to Markov chain
eventHandler :: DiscordThreadState -> DiscordHandle -> Event -> IO ()
eventHandler dts dis (MessageCreate m)
  | not (fromBot m) && isPing (messageText m) =
    void $
    restCall dis (R.CreateReaction (messageChannel m, messageId m) "hearts")
  | not (fromBot m) = do
    withMVar (dtsSqliteConnection dts) $ \dbConn -> do
      currentUser <- tryReadMVar $ dtsCurrentUser dts
      catch
        (Sqlite.withTransaction dbConn $ do
           logEntry dts $ LogEntry "DISCORD" $ messageText m
             -- TODO(#109): DiscordThread doesn't cache the guilds
           guild <-
             case messageGuild m of
               Just guildId' -> do
                 resGuild <- restCall dis $ R.GetGuild guildId'
                 case resGuild of
                   Right guild' -> return $ Just guild'
                   Left restError -> do
                     logEntry dts $ LogEntry "DISCORD" $ T.pack $ show restError
                     return Nothing
               Nothing -> do
                 logEntry dts $
                   LogEntry "DISCORD" "[WARN] Message was not sent in a Guild"
                 return Nothing
           guildMember <-
             case guild of
               Just guild' -> do
                 res <-
                   restCall dis $
                   R.GetGuildMember (guildId guild') (userId $ messageAuthor m)
                 case res of
                   Right guildMember' -> return $ Just guildMember'
                   Left restError -> do
                     logEntry dts $ LogEntry "DISCORD" $ T.pack $ show restError
                     return Nothing
               Nothing -> undefined
           logMessage
             dbConn
             (messageGuild m)
             (messageChannel m)
             (userId $ messageAuthor m) $
             messageText m
           addMarkovSentence dbConn $ messageText m
           case parseCommandPipe
                  (CallPrefix "$")
                  (PipeSuffix "|")
                  (messageText m) of
             [] ->
               when
                 (isJust $
                  find (\u -> Just (userId u) == (userId <$> currentUser)) $
                  messageMentions m) $ do
                 markovResponse <- genMarkovSentence dbConn
                 void $
                   restCall dis $
                   R.CreateMessage (messageChannel m) $
                   T.pack $
                   printf
                     "<@!%d> %s"
                     ((fromIntegral $ userId $ messageAuthor m) :: Word64)
                     markovResponse
             pipe -> do
               evalResult <-
                 runExceptT $
                 evalStateT (runEvalT $ evalCommandPipe pipe) $
                 EvalContext
                   { ecVars = M.fromList []
                   , ecSqliteConnection = dbConn
                   , ecPlatformContext =
                       Edc
                         EvalDiscordContext
                           { edcAuthor = messageAuthor m
                           , edcGuild = guild
                           , edcRoles =
                               concat $
                               maybeToList (memberRoles <$> guildMember)
                           }
                   , ecLogQueue = dtsLogQueue dts
                   , ecManager = dtsManager dts
                   }
               case evalResult of
                 Right commandResponse ->
                   void $
                   restCall dis $
                   R.CreateMessage (messageChannel m) commandResponse
                 Left (EvalError userMsg) ->
                   void $
                   restCall dis (R.CreateMessage (messageChannel m) userMsg))
        (\e ->
           logEntry dts $ LogEntry "SQLITE" $ T.pack $ show (e :: SomeException))
    pure ()
eventHandler _ _ _ = pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower

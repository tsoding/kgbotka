{-# LANGUAGE OverloadedStrings #-}

module KGBotka.DiscordThread
  ( DiscordThreadParams(..)
  , discordThread
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Eval
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import Discord
import qualified Discord.Requests as R
import Discord.Types
import KGBotka.Command
import KGBotka.Config
import KGBotka.Eval
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Queue
import KGBotka.Sqlite
import qualified Network.HTTP.Client as HTTP

data DiscordThreadParams = DiscordThreadParams
  { dtpConfig :: !(Maybe ConfigDiscord)
  , dtpLogQueue :: !(WriteQueue LogEntry)
  , dtpSqliteFileName :: !FilePath
  , dtpManager :: !HTTP.Manager
  }

data DiscordThreadState = DiscordThreadState
  { dtsLogQueue :: !(WriteQueue LogEntry)
  , dtsSqliteConnection :: Sqlite.Connection
  , dtsManager :: !HTTP.Manager
  }

instance ProvidesLogging DiscordThreadState where
  logQueue = dtsLogQueue

instance ProvidesLogging DiscordThreadParams where
  logQueue = dtpLogQueue

discordThread :: DiscordThreadParams -> IO ()
discordThread dtp =
  case dtpConfig dtp of
    Just config ->
      withConnectionAndPragmas (dtpSqliteFileName dtp) $ \sqliteConnection -> do
        userFacingError <-
          runDiscord $
          def
            { discordToken = configDiscordToken config
            , discordOnEvent =
                eventHandler $
                DiscordThreadState
                  { dtsLogQueue = dtpLogQueue dtp
                  , dtsSqliteConnection = sqliteConnection
                  , dtsManager = dtpManager dtp
                  }
            }
        logEntry dtp $ LogEntry "DISCORD" userFacingError
    Nothing ->
      logEntry dtp $
      LogEntry "DISCORD" "[ERROR] Discord configuration not found"

-- TODO(#96): Discord messages are not logged
-- TODO(#97): Discord messages do not contribute to Markov chain
eventHandler :: DiscordThreadState -> DiscordHandle -> Event -> IO ()
eventHandler dts dis (MessageCreate m)
  | not (fromBot m) && isPing (messageText m) =
    void $
    restCall dis (R.CreateReaction (messageChannel m, messageId m) "hearts")
  | not (fromBot m) = do
    let dbConn = dtsSqliteConnection dts
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
         addMarkovSentence (dtsSqliteConnection dts) $ messageText m
         evalResult <-
           runExceptT $
           evalStateT
             (runEvalT $
              evalCommandPipe $
              parseCommandPipe (CallPrefix "$") (PipeSuffix "|") (messageText m)) $
           EvalContext
             { ecVars = M.fromList []
             , ecSqliteConnection = dbConn
             , ecPlatformContext =
                 Edc
                   EvalDiscordContext
                     { edcAuthor = messageAuthor m
                     , edcGuild = guild
                     , edcRoles =
                         concat $ maybeToList (memberRoles <$> guildMember)
                     }
             , ecLogQueue = dtsLogQueue dts
             , ecManager = dtsManager dts
             }
         case evalResult of
           Right commandResponse ->
             void $
             restCall dis $ R.CreateMessage (messageChannel m) commandResponse
           Left (EvalError userMsg) ->
             void $ restCall dis (R.CreateMessage (messageChannel m) userMsg))
      (\e ->
         logEntry dts $ LogEntry "SQLITE" $ T.pack $ show (e :: Sqlite.SQLError))
    pure ()
eventHandler _ _ _ = pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower

{-# LANGUAGE OverloadedStrings #-}

module KGBotka.DiscordThread
  ( DiscordThreadParams(..)
  , discordThread
  ) where

import Control.Concurrent
import Control.Concurrent.STM
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
import KGBotka.Settings
import qualified Network.HTTP.Client as HTTP
import Text.Printf

data DiscordThreadParams = DiscordThreadParams
  { dtpConfig :: !(Maybe ConfigDiscord)
  , dtpLogQueue :: !(WriteQueue LogEntry)
  , dtpSqliteConnection :: !(MVar Sqlite.Connection)
  , dtpManager :: !HTTP.Manager
  , dtpFridayGistUpdateRequired :: !(MVar ())
  , dtpMarkovQueue :: !(WriteQueue MarkovCommand)
  }

data DiscordThreadState = DiscordThreadState
  { dtsLogQueue :: !(WriteQueue LogEntry)
  , dtsSqliteConnection :: !(MVar Sqlite.Connection)
  , dtsManager :: !HTTP.Manager
  -- TODO(#173): replace dtsCurrentUser :: !(MVar User) with !(Maybe User)
  , dtsCurrentUser :: !(MVar User)
  , dtsFridayGistUpdateRequired :: !(MVar ())
  , dtsMarkovQueue :: !(WriteQueue MarkovCommand)
  }

instance ProvidesLogging DiscordThreadState where
  logEntry dts = logEntry $ dtsLogQueue dts

instance ProvidesLogging DiscordThreadParams where
  logEntry dtp = logEntry $ dtpLogQueue dtp

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
              , dtsFridayGistUpdateRequired = dtpFridayGistUpdateRequired dtp
              , dtsMarkovQueue = dtpMarkovQueue dtp
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

eventHandler :: DiscordThreadState -> DiscordHandle -> Event -> IO ()
eventHandler dts _ (MessageReactionAdd _) = do
  logEntry dts $ LogEntry "DISCORD" "------------------------------"
  logEntry dts $ LogEntry "DISCORD" "REACTION HAS BEEN ADDED"
  logEntry dts $ LogEntry "DISCORD" "------------------------------"
eventHandler dts _ (MessageReactionRemove _) = do
  logEntry dts $ LogEntry "DISCORD" "------------------------------"
  logEntry dts $ LogEntry "DISCORD" "REACTION HAS BEEN REMOVED"
  logEntry dts $ LogEntry "DISCORD" "------------------------------"
eventHandler dts dis (MessageCreate m)
  | not (fromBot m) && isPing (messageText m) =
    void $
    restCall dis (R.CreateReaction (messageChannel m, messageId m) "hearts")
  | not (fromBot m) = do
    withMVar (dtsSqliteConnection dts) $ \dbConn ->
      catch
        (Sqlite.withTransaction dbConn $ do
           logEntry dts $
             LogEntry "DISCORD" $
             T.pack $ printf "%s: %s" (show $ messageAuthor m) (messageText m)
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
               Nothing -> do
                 logEntry dts $
                   LogEntry
                     "DISCORD"
                     "[WARN] Recieved a message outside of a Guild"
                 return Nothing
           logMessage
             dbConn
             (messageGuild m)
             (messageChannel m)
             (userId $ messageAuthor m)
             (userName $ messageAuthor m) $
             messageText m
           atomically $
             writeQueue (dtsMarkovQueue dts) $ NewSentence $ messageText m
           settings <- fetchSettings dbConn
           case parseCommandPipe
                  (settingsCallPrefix settings)
                  (PipeSuffix "|")
                  (messageText m) of
             [] -> return ()
               -- when
               --   (isJust $
               --    find (\u -> Just (userId u) == (userId <$> currentUser)) $
               --    messageMentions m) $ do
               --   markovResponse <- genMarkovSentence dbConn
               --   void $
               --     restCall dis $
               --     R.CreateMessage (messageChannel m) $
               --     T.pack $
               --     printf
               --       "<@!%d> %s"
               --       ((fromIntegral $ userId $ messageAuthor m) :: Word64)
               --       markovResponse
             pipe -> do
               evalResult <-
                 runExceptT $
                 evalStateT (runEvalT $ evalCommandPipe pipe) $
                 EvalContext
                   { ecVars = M.empty
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
                   , ecFridayGistUpdateRequired =
                       dtsFridayGistUpdateRequired dts
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  , ConfigTwitch(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Aeson
import Data.Foldable
import qualified Database.SQLite.Simple as Sqlite
import KGBotka.Config
import KGBotka.DiscordThread
import KGBotka.GithubThread
import KGBotka.Log
import KGBotka.Markov
import KGBotka.Migration
import KGBotka.Queue
import KGBotka.Repl
import KGBotka.TwitchThread
import qualified Network.HTTP.Client.TLS as TLS
import System.Environment
import System.Exit
import System.IO

-- TODO(#143): Periodic evaluation
withForkIOs :: [IO ()] -> ([ThreadId] -> IO b) -> IO b
withForkIOs ios = bracket (traverse forkIO ios) (traverse_ killThread)

mainWithArgs :: [String] -> IO ()
mainWithArgs (configPath:databasePath:_) = do
  putStrLn $ "Your configuration file is " <> configPath
  eitherDecodeFileStrict configPath >>= \case
    Right config -> do
      replQueue <- atomically newTQueue
      rawLogQueue <- atomically newTQueue
      markovCmdQueue <- atomically newTQueue
      manager <- TLS.newTlsManager
      sqliteConnection <- newEmptyMVar
      fridayGistUpdateRequired <- newMVar ()
      currentRetrainPage <- newMVar Nothing
      -- TODO(#67): there is no supavisah that restarts essential threads on crashing
      Sqlite.withConnection databasePath $ \dbConn -> do
        Sqlite.withTransaction dbConn $ migrateDatabase dbConn kgbotkaMigrations
        putMVar sqliteConnection dbConn
        withForkIOs
          [ twitchThread $
            TwitchThreadParams
              { ttpReplQueue = ReadQueue replQueue
              , ttpSqliteConnection = sqliteConnection
              , ttpLogQueue = WriteQueue rawLogQueue
              , ttpManager = manager
              , ttpConfig = configTwitch config
              , ttpFridayGistUpdateRequired = fridayGistUpdateRequired
              , ttpMarkovQueue = WriteQueue markovCmdQueue
              }
          , discordThread $
            DiscordThreadParams
              { dtpConfig = configDiscord config
              , dtpLogQueue = WriteQueue rawLogQueue
              , dtpSqliteConnection = sqliteConnection
              , dtpManager = manager
              , dtpFridayGistUpdateRequired = fridayGistUpdateRequired
              , dtpMarkovQueue = WriteQueue markovCmdQueue
              }
          , loggingThread "kgbotka.log" $ ReadQueue rawLogQueue
          , githubThread $
            GithubThreadParams
              { gtpSqliteConnection = sqliteConnection
              , gtpManager = manager
              , gtpLogQueue = WriteQueue rawLogQueue
              , gtpConfig = configGithub config
              , gtpUpdateRequired = fridayGistUpdateRequired
              }
          , markovThread $
            MarkovThreadParams
              { mtpSqliteConnection = sqliteConnection
              , mtpLogQueue = WriteQueue rawLogQueue
              , mtpCmdQueue = ReadQueue markovCmdQueue
              , mtpPageSize = 1000
              , mtpCurrentPage = currentRetrainPage
              }
          ] $ \_ ->
          backdoorThread $
          BackdoorThreadParams
            { btpSqliteConnection = sqliteConnection
            , btpCommandQueue = WriteQueue replQueue
            , btpTwitchClientId = configTwitchClientId <$> configTwitch config
            , btpManager = manager
            , btpLogQueue = WriteQueue rawLogQueue
            , btpPort = 6969 -- TODO(#63): backdoor port is hardcoded
            , btpMarkovQueue = WriteQueue markovCmdQueue
            , btpCurrentRetrainPage = currentRetrainPage
            }
      putStrLn "Done"
    Left errorMessage -> error errorMessage
mainWithArgs _ = do
  hPutStrLn stderr "[ERROR] Not enough arguments provided"
  hPutStrLn stderr "Usage: ./kgbotka <config.json> <database.db>"
  exitFailure

main :: IO ()
main = getArgs >>= mainWithArgs

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
import qualified Data.Set as S
import qualified Database.SQLite.Simple as Sqlite
import KGBotka.Config
import KGBotka.DiscordThread
import KGBotka.GithubThread
import KGBotka.Log
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
      joinedChannels <- atomically $ newTVar S.empty
      manager <- TLS.newTlsManager
      sqliteConnection <- newEmptyMVar
      fridayGistUpdateRequired <- newMVar ()
      -- TODO(#67): there is no supavisah that restarts essential threads on crashing
      Sqlite.withConnection databasePath $ \dbConn -> do
        Sqlite.withTransaction dbConn $ migrateDatabase dbConn kgbotkaMigrations
        putMVar sqliteConnection dbConn
        withForkIOs
          [ twitchThread $
            TwitchThreadParams
              { ttpReplQueue = ReadQueue replQueue
              , ttpChannels = joinedChannels
              , ttpSqliteConnection = sqliteConnection
              , ttpLogQueue = WriteQueue rawLogQueue
              , ttpManager = manager
              , ttpConfig = configTwitch config
              , ttpFridayGistUpdateRequired = fridayGistUpdateRequired
              }
          , discordThread $
            DiscordThreadParams
              { dtpConfig = configDiscord config
              , dtpLogQueue = WriteQueue rawLogQueue
              , dtpSqliteConnection = sqliteConnection
              , dtpManager = manager
              , dtpFridayGistUpdateRequired = fridayGistUpdateRequired
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
          ] $ \_ ->
          backdoorThread $
          BackdoorThreadParams
            { btpChannels = joinedChannels
            , btpSqliteConnection = sqliteConnection
            , btpCommandQueue = WriteQueue replQueue
            , btpTwitchClientId = configTwitchClientId <$> configTwitch config
            , btpManager = manager
            , btpLogQueue = WriteQueue rawLogQueue
            , btpPort = 6969 -- TODO(#63): backdoor port is hardcoded
            }
      putStrLn "Done"
    Left errorMessage -> error errorMessage
mainWithArgs _ = do
  hPutStrLn stderr "[ERROR] Not enough arguments provided"
  hPutStrLn stderr "Usage: ./kgbotka <config.json> <database.db>"
  exitFailure

main :: IO ()
main = getArgs >>= mainWithArgs

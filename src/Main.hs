{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Database.SQLite.Simple.QQ
import KGBotka.Config
import KGBotka.DiscordThread
import KGBotka.Log
import KGBotka.Migration
import KGBotka.Queue
import KGBotka.Repl
import KGBotka.TwitchThread
import qualified Network.HTTP.Client.TLS as TLS
import System.Environment
import System.Exit
import System.IO

migrations :: [Migration]
migrations =
  [ Migration
      [sql|CREATE TABLE Command (
             id INTEGER PRIMARY KEY,
             code TEXT NOT NULL
           );|]
  , Migration
      [sql|CREATE TABLE CommandName (
             name TEXT NOT NULL,
             commandId INTEGER NOT NULL REFERENCES Command(id) ON DELETE CASCADE,
             UNIQUE(name) ON CONFLICT REPLACE
           );|]
  , Migration
      [sql|CREATE TABLE TwitchRoles (
             id INTEGER PRIMARY KEY,
             name TEXT NOT NULL UNIQUE
           );|]
  , Migration
      [sql|CREATE TABLE TwitchUserRoles (
             userId TEXT NOT NULL,
             roleId INTEGER NOT NULL REFERENCES TwitchRoles(id) ON DELETE CASCADE,
             UNIQUE(userId, roleId) ON CONFLICT IGNORE
           );|]
  , Migration
      [sql|CREATE TABLE FridayVideo (
             id INTEGER PRIMARY KEY,
             submissionText TEXT NOT NULL,
             submissionTime DATETIME NOT NULL,
             authorTwitchId TEXT NOT NULL,
             authorTwitchName TEXT NOT NULL,
             watchedAt DATETIME
           );|]
  , Migration
      [sql|CREATE TABLE TwitchLog (
             id INTEGER PRIMARY KEY,
             channel TEXT NOT NULL,
             senderTwitchId TEXT NOT NULL,
             senderTwitchName TEXT NOT NULL,
             senderTwitchDisplayName TEXT,
             senderTwitchRoles TEXT NOT NULL,
             senderTwitchBadgeRoles TEXT NOT NULL,
             message TEXT NOT NULL,
             messageTime DATETIME DEFAULT (datetime('now')) NOT NULL
           )|]
  , Migration
      [sql|CREATE TABLE Markov (
             event1 TEXT NOT NULL,
             event2 TEXT NOT NULL,
             n INTEGER NOT NULL,
             UNIQUE (event1, event2) ON CONFLICT REPLACE
           );
           CREATE INDEX markov_event1_index ON Markov (event1);|]
  , Migration
      [sql|ALTER TABLE Command
           ADD COLUMN user_cooldown_ms INTEGER NOT NULL DEFAULT 0;|]
  , Migration
      [sql|CREATE TABLE CommandLog (
             userTwitchId TEXT,
             userDiscordId TEXT,
             commandId INTEGER NOT NULL,
             commandArgs TEXT NOT NULL,
             timestamp DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
           );|]
  , Migration
      [sql|CREATE TABLE AsciifyUrlCache(
             url TEXT NOT NULL,
             image TEXT NOT NULL,
             UNIQUE (url) ON CONFLICT REPLACE
           );|]
  , Migration
      [sql|CREATE TABLE BttvEmotes (
              name TEXT NOT NULL,
              channel TEXT DEFAULT NULL,
              imageUrl TEXT NOT NULL
           );|]
  , Migration
      [sql|CREATE TABLE FfzEmotes (
              name TEXT NOT NULL,
              channel TEXT DEFAULT NULL,
              imageUrl TEXT NOT NULL
           );|]
  , Migration
      [sql|CREATE TABLE DiscordLog (
             id INTEGER PRIMARY KEY,
             guildId TEXT,
             channelId TEXT NOT NULL,
             senderDiscordId TEXT NOT NULL,
             message TEXT NOT NULL,
             messageTime DATETIME DEFAULT (datetime('now')) NOT NULL
           )|]
  , Migration
      [sql|ALTER TABLE FridayVideo RENAME
           COLUMN authorTwitchName
           TO authorDisplayName |]
  ]

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
      -- TODO(#67): there is no supavisah that restarts essential threads on crashing
      Sqlite.withConnection databasePath $ \dbConn -> do
        Sqlite.withTransaction dbConn $ migrateDatabase dbConn migrations
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
              }
          , discordThread $
            DiscordThreadParams
              { dtpConfig = configDiscord config
              , dtpLogQueue = WriteQueue rawLogQueue
              , dtpSqliteConnection = sqliteConnection
              , dtpManager = manager
              }
          , loggingThread "kgbotka.log" $ ReadQueue rawLogQueue
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

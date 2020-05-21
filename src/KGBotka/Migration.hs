{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Migration
  ( migrateDatabase
  , Migration(..)
  , kgbotkaMigrations
  ) where

import Data.String

import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ

newtype Migration = Migration
  { migrationQuery :: Query
  } deriving (Show)

instance IsString Migration where
  fromString = Migration . fromString

instance FromRow Migration where
  fromRow = fromString <$> field

-- TODO(#81): Migration comparison function is very error prone
--   We need some kind of query normalization algorithm which is
--   semantic-insensitive
instance Eq Migration where
  (==) = (==) `on` (T.unwords . T.words . fromQuery . migrationQuery)

applyMigration :: Connection -> Migration -> IO ()
applyMigration conn (Migration q) = do
  execute_ conn q
  executeNamed
    conn
    "INSERT INTO Migrations (migrationQuery)\
    \VALUES (:migrationQuery)"
    [":migrationQuery" := fromQuery q]

createMigrationTablesIfNeeded :: Connection -> IO ()
createMigrationTablesIfNeeded conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS Migrations (\
    \  id INTEGER PRIMARY KEY,              \
    \  migrationQuery TEXT NOT NULL         \
    \)"

filterUnappliedMigrations :: Connection -> [Migration] -> IO [Migration]
filterUnappliedMigrations conn migrations = do
  appliedMigrations <- query_ conn "SELECT migrationQuery FROM Migrations"
  maybe
    (error
       "Inconsistent migrations state! \
       \List of already applied migrations \
       \is not a prefix of required migrations.")
    return $
    stripPrefix appliedMigrations migrations

migrateDatabase :: Connection -> [Migration] -> IO ()
migrateDatabase conn migrations = do
  createMigrationTablesIfNeeded conn
  unappliedMigrations <- filterUnappliedMigrations conn migrations
  traverse_ (applyMigration conn) unappliedMigrations

kgbotkaMigrations :: [Migration]
kgbotkaMigrations =
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
  -- TODO(#126): There is no way to find out from where the video was submitted (Twitch or Discord) based on the data from FridayVideo
  , Migration
      [sql|CREATE TABLE FridayVideo (
             id INTEGER PRIMARY KEY,
             submissionText TEXT NOT NULL,
             submissionTime DATETIME NOT NULL,
             authorId TEXT NOT NULL,
             authorDisplayName TEXT NOT NULL,
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
      [sql|CREATE TABLE Settings (
             name TEXT NOT NULL,
             value TEXT NOT NULL
           )|]
  , Migration
      [sql|CREATE TABLE JoinedTwitchChannels (
             name TEXT NOT NULL,
             UNIQUE (name) ON CONFLICT IGNORE
           )|]
  , Migration
      [sql|ALTER TABLE Command
           ADD COLUMN times INT NOT NULL DEFAULT 0;|]
  , Migration [sql|ALTER TABLE TwitchLog RENAME TO TwitchLogOld;|]
  , Migration
      [sql|CREATE TABLE TwitchLog (
             id INTEGER PRIMARY KEY,
             channel TEXT NOT NULL,
             senderTwitchId TEXT,
             senderTwitchName TEXT NOT NULL,
             senderTwitchDisplayName TEXT,
             senderTwitchRoles TEXT NOT NULL,
             senderTwitchBadgeRoles TEXT NOT NULL,
             message TEXT NOT NULL,
             messageTime DATETIME DEFAULT (datetime('now')) NOT NULL);|]
  , Migration
      [sql|INSERT INTO TwitchLog (id, channel, senderTwitchId, senderTwitchName, senderTwitchDisplayName, senderTwitchRoles, senderTwitchBadgeRoles, message, messageTime) SELECT id, channel, senderTwitchId, senderTwitchName, senderTwitchDisplayName, senderTwitchRoles, senderTwitchBadgeRoles, message, messageTime FROM TwitchLogOld;|]
  , Migration [sql|DROP TABLE TwitchLogOld;|]
  , Migration [sql|ALTER TABLE DiscordLog RENAME TO DiscordLogOld;|]
  , Migration
      [sql|CREATE TABLE DiscordLog (
             id INTEGER PRIMARY KEY,
             guildId TEXT,
             channelId TEXT NOT NULL,
             senderDiscordId TEXT,
             senderDiscordDisplayName TEXT,
             message TEXT NOT NULL,
             messageTime DATETIME DEFAULT (datetime('now')) NOT NULL
           );|]
  , Migration
      [sql|INSERT INTO DiscordLog (id, guildId, channelId, senderDiscordId, senderDiscordDisplayName, message, messageTime) SELECT id, guildId, channelId, senderDiscordId, NULL, message, messageTime FROM DiscordLogOld;|]
  , Migration [sql|DROP TABLE DiscordLogOld;|]
  , Migration [sql|CREATE TABLE RoleAssignMessages (messageId INTEGER NOT NULL UNIQUE);|]
  , Migration [sql|CREATE TABLE RoleEmoteAssoc (
                     emoteId INTEGER NOT NULL,
                     roleId INTEGER NOT NULL,
                     messageId INTEGER NOT NULL,
                     UNIQUE(emoteId, messageId)
                   );|]
  ]

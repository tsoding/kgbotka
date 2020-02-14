{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Migration
  ( migrateDatabase
  , Migration(..)
  ) where

import Data.String

import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Text as T
import Database.SQLite.Simple

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

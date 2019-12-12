{-# LANGUAGE OverloadedStrings #-}

module Migration where

import Data.String

import Database.SQLite.Simple
import qualified Data.Text as T
import Data.Function
import Data.Foldable
import Data.List

newtype Migration = Migration
  { migrationQuery :: Query
  }

instance IsString Migration where
  fromString = Migration . fromString

instance FromRow Migration where
  fromRow = fromString <$> field

instance Eq Migration where
  (==) = (==) `on` (T.unwords . T.words . fromQuery . migrationQuery)

applyMigration :: Connection -> Migration -> IO ()
applyMigration conn (Migration q) = do
  execute_ conn q
  executeNamed
    conn
    "INSERT INTO Migrations (migrationQuery)\
    \VALUES (:migrationQuery)"
    [":migrationQuery" := (fromQuery q)]

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

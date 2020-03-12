{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Sqlite
  ( withConnectionAndPragmas
  , ProvidesDatabase(..)
  ) where

import qualified Database.SQLite.Simple as Sqlite

-- | Makes Sqlite connection but with essential, for the application,
-- settings. Should be used instead of the regular withConnection that
-- comes with sqlite-simple package
withConnectionAndPragmas :: FilePath -> (Sqlite.Connection -> IO ()) -> IO ()
withConnectionAndPragmas filePath body =
  Sqlite.withConnection filePath $ \conn -> do
    Sqlite.execute_ conn "PRAGMA foreign_keys=ON"
    Sqlite.execute_ conn "PRAGMA journal_mode=WAL"
    body conn

class ProvidesDatabase pd where
  getSqliteConnection :: pd -> Sqlite.Connection

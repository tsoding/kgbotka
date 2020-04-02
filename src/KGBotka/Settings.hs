{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Settings
  ( Settings(..)
  , fetchSettings
  ) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ

-- FIXME(#124): Difference between KGBotka.Settings and KGBotka.Config is not clear
newtype Settings = Settings
  { settingsFridayGithubGistId :: Maybe T.Text
  } deriving (Show)

deserializeSettings :: [(T.Text, T.Text)] -> Settings
deserializeSettings settingsMap =
  Settings
    {settingsFridayGithubGistId = lookup "fridayGithubGistId" settingsMap}

fetchSettings :: Connection -> IO Settings
fetchSettings dbConn =
  deserializeSettings <$> queryNamed dbConn [sql|SELECT * FROM Settings;|] []

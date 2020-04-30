{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Settings
  ( Settings(..)
  , fetchSettings
  ) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Data.Maybe
import KGBotka.Command

-- FIXME(#124): Difference between KGBotka.Settings and KGBotka.Config is not clear
data Settings = Settings
  { settingsFridayGithubGistId :: Maybe T.Text
  , settingsCallPrefix :: CallPrefix
  } deriving (Show)

deserializeSettings :: [(T.Text, T.Text)] -> Settings
deserializeSettings settingsMap =
  Settings
    { settingsFridayGithubGistId = lookup "fridayGithubGistId" settingsMap
    , settingsCallPrefix =
        CallPrefix $ fromMaybe "$" $ lookup "callPrefix" settingsMap
    }

fetchSettings :: Connection -> IO Settings
fetchSettings dbConn =
  deserializeSettings <$> queryNamed dbConn [sql|SELECT * FROM Settings;|] []

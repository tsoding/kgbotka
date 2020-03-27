module KGBotka.Settings
  ( Settings(..)
  , fetchSettings
  ) where

import qualified Data.Text as T
import Database.SQLite.Simple

-- FIXME(#124): Difference between KGBotka.Settings and KGBotka.Config is not clear
newtype Settings = Settings
  { settingsFridayGithubGistId :: Maybe T.Text
  }

-- FIXME(#125): fetchSettings is not implemented
fetchSettings :: Connection -> IO Settings
fetchSettings _ = return $ Settings Nothing

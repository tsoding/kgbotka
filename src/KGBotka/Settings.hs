module KGBotka.Settings where

import qualified Data.Text as T
import Database.SQLite.Simple

-- FIXME: Difference between KGBotka.Settings and KGBotka.Config is not clear

data Settings = Settings
  { settingsFridayGithubGistId :: !(Maybe T.Text)
  }

-- FIXME: fetchSettings is not implemented
fetchSettings :: Connection -> IO Settings
fetchSettings = undefined

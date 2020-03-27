module KGBotka.GithubThread
  ( githubThread
  , GithubThreadParams(..)
  ) where

import Control.Concurrent
import qualified Database.SQLite.Simple as Sqlite
import Network.HTTP.Client
import KGBotka.Config
import KGBotka.Queue
import KGBotka.Log

data GithubThreadParams = GithubThreadParams
  { gtpSqliteConnection :: !(MVar Sqlite.Connection)
  , gtpManager :: !Manager
  , gtpLogQueue :: !(WriteQueue LogEntry)
  , gtpConfig :: !(Maybe ConfigGithub)
  }

instance ProvidesLogging GithubThreadParams where
  logQueue = gtpLogQueue

data GithubThreadState = GithubThreadState
  { gtsSqliteConnection :: !(MVar Sqlite.Connection)
  , gtsManager :: !Manager
  , gtsLogQueue :: !(WriteQueue LogEntry)
  , gtsConfig :: ConfigGithub
  }

instance ProvidesLogging GithubThreadState where
  logQueue = gtsLogQueue

githubThread :: GithubThreadParams -> IO ()
githubThread gtp@GithubThreadParams {gtpConfig = Just config} =
  githubThreadLoop
    GithubThreadState
      { gtsSqliteConnection = gtpSqliteConnection gtp
      , gtsManager = gtpManager gtp
      , gtsLogQueue = gtpLogQueue gtp
      , gtsConfig = config
      }
githubThread gtp =
  logEntry gtp $
  LogEntry "GITHUB" "[ERROR] GitHub configuration is not provided"

githubThreadLoop :: GithubThreadState -> IO ()
githubThreadLoop gts = do
  threadDelay $ 60 * 1000 * 1000
  logEntry gts $ LogEntry "GITHUB" "Updating Friday Video Queue gist"
  githubThreadLoop gts

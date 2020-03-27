{-# LANGUAGE OverloadedStrings #-}

module KGBotka.GithubThread
  ( githubThread
  , GithubThreadParams(..)
  ) where

import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import KGBotka.Config
import KGBotka.Friday
import KGBotka.Log
import KGBotka.Queue
import KGBotka.Settings
import Network.HTTP.Client

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
  fridayGist <-
    withMVar (gtsSqliteConnection gts) $ \conn ->
      Sqlite.withTransaction conn $
      runMaybeT $ do
        gistId <- MaybeT $ settingsFridayGithubGistId <$> fetchSettings conn
        gistText <- lift $ renderAllQueues <$> fetchAllQueues conn
        return (gistId, gistText)
  case fridayGist of
    Just (gistId, gistText) -> do
      logEntry gts $ LogEntry "GITHUB" "Updating Friday Video Queue gist..."
      updateGistText (gtsManager gts) gistId gistText
    Nothing ->
      logEntry gts $
      LogEntry
        "GITHUB"
        "[WARN] Tried to update Friday Video Queue, \
        \but the gist id is not setup"
  githubThreadLoop gts

-- FIXME: updateGistText is not implemented
updateGistText :: Manager -> T.Text -> T.Text -> IO ()
updateGistText _ _ _ = return ()

-- FIXME: renderAllQueues is not implemented
renderAllQueues :: M.Map AuthorId [FridayVideo] -> T.Text
renderAllQueues _ = ""

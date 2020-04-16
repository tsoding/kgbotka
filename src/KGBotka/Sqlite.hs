module KGBotka.Sqlite where

import Control.Concurrent
import Database.SQLite.Simple

withLockedTransaction :: MVar Connection -> (Connection -> IO a) -> IO a
withLockedTransaction mvarDbConn action =
  withMVar mvarDbConn $ \dbConn -> withTransaction dbConn (action dbConn)

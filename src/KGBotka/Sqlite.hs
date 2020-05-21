module KGBotka.Sqlite where

import Control.Concurrent
import Database.SQLite.Simple

-- TODO: make withLockedTransaction to always handle Exceptions from Sqlite library
--   Give the user some endpoints to log the errors
withLockedTransaction :: MVar Connection -> (Connection -> IO a) -> IO a
withLockedTransaction mvarDbConn action =
  withMVar mvarDbConn $ \dbConn -> withTransaction dbConn (action dbConn)

module KGBotka.Log (loggingThread) where

import KGBotka.Queue
import qualified Data.Text as T
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time

loggingThread :: FilePath -> ReadQueue T.Text -> IO ()
loggingThread logFilePath messageQueue =
  withFile logFilePath AppendMode loop
  where
    loop logHandle = do
      threadDelay 10000 -- to prevent busy looping
      messages <- atomically $ flushQueue messageQueue
      timestamp <-
        formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") <$>
        getCurrentTime
      mapM_
        (\message ->
           hPutStrLn logHandle $ "[" <> timestamp <> "] " <> T.unpack message)
        messages
      hFlush logHandle
      loop logHandle

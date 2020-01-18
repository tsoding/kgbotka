module KGBotka.Queue
  ( WriteQueue(..)
  , writeQueue
  , ReadQueue(..)
  , readQueue
  , tryReadQueue
  , toWriteQueue
  , toReadQueue
  , flushQueue
  ) where

import Control.Concurrent.STM

newtype WriteQueue a = WriteQueue
  { getWriteQueue :: TQueue a
  }

{-# INLINE writeQueue #-}
writeQueue :: WriteQueue a -> a -> STM ()
writeQueue = writeTQueue . getWriteQueue

newtype ReadQueue a = ReadQueue
  { getReadQueue :: TQueue a
  }

{-# INLINE readQueue #-}
readQueue :: ReadQueue a -> STM a
readQueue = readTQueue . getReadQueue

{-# INLINE tryReadQueue #-}
tryReadQueue :: ReadQueue a -> STM (Maybe a)
tryReadQueue = tryReadTQueue . getReadQueue

{-# INLINE flushQueue #-}
flushQueue :: ReadQueue a -> STM [a]
flushQueue = flushTQueue . getReadQueue

{-# INLINE toWriteQueue #-}
toWriteQueue :: ReadQueue a -> WriteQueue a
toWriteQueue = WriteQueue . getReadQueue

{-# INLINE toReadQueue #-}
toReadQueue :: WriteQueue a -> ReadQueue a
toReadQueue = ReadQueue . getWriteQueue

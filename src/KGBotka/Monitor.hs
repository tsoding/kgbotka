module KGBotka.Monitor where

import Control.Concurrent

-- Who needs OCaml LOOOOOOL

newtype T = T (MVar ())

new :: IO T
new = T <$> newEmptyMVar

wait :: T -> IO ()
wait (T mvar) = takeMVar mvar

notify :: T -> IO ()
notify (T mvar) = putMVar mvar ()

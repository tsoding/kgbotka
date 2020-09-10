{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Data.Functor
import Control.Monad
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)
import System.IO
import System.Exit (exitFailure)

-- FIXME:
-- If client receives exactly {chunkSize} number of bytes
-- it tries to poll socket again just in case there's more
-- data waiting. And if there is nothing (server sent
-- exactly {cunkSize} bytes for example) it blocks untill next
-- message from the server. Idk how to solve it for now,
-- but at least this solution is already better and covers
-- more cases than the one before.
chunkSize = 2048

replState :: Socket -> IO ()
replState s = do
  line <- recv s chunkSize
  unless (C.null line) $ do
    putStr $ C.unpack line
    if C.length line == chunkSize
    then replState s
    else do
      hFlush stdout
      line <- getLine
      void . send s . C.concat $ [C.pack line, "\n"]
      replState s

csrfAuthState :: Socket -> IO ()
csrfAuthState s = do
  line <- recv s 2048
  if C.isPrefixOf "CSRF => " line then do
    void . send s . C.concat $ [C.drop 8 . head . C.lines $ line, "\n"]
    putStrLn "Authorized!"
    replState s
  else do
    putStrLn "Could not receive the CSRF token, aborting"
    exitFailure

main :: IO ()
main =
  runTCPClient "127.0.0.1" "6969" $ \s -> do
    putStrLn "Connected!"
    queue <- atomically newTQueue
    csrfAuthState s
    putStrLn "Connection closed, have a great day!"

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client =
  withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

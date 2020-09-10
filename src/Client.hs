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

replState :: Socket -> IO ()
replState s = do
  line <- recv s 2048
  unless (C.null line) $ do
    putStr $ C.unpack line
    hFlush stdout
    line <- getLine
    void . send s $ C.concat [C.pack line, C.pack "\n"]
    replState s

csrfAuthState :: Socket -> IO ()
csrfAuthState s = do
  line <- recv s 2048
  if C.isPrefixOf "CSRF => " line then do
    void . send s $ C.drop 8 line
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

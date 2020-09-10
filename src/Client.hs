module Main where

import qualified Control.Exception as E
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent.STM
import Data.Functor
import System.IO
import Control.Concurrent

bufferSize :: Int
bufferSize = 1024

readThread :: TQueue C.ByteString -> Socket -> IO ()
readThread queue s = do
  chunk <- recv s bufferSize
  atomically $ writeTQueue queue chunk
  readThread queue s

replState :: TQueue C.ByteString -> Socket -> IO ()
replState queue s = do
  threadDelay 10000
  chunks <- atomically $ flushTQueue queue
  putStr $ C.unpack $ C.concat chunks
  hFlush stdout
  line <- getLine
  void $ send s $ C.concat [C.pack line, C.pack "\n"]
  replState queue s

csrfAuthState :: [C.ByteString] -> TQueue C.ByteString -> Socket -> IO ()
csrfAuthState chunks queue s = do
  chunks' <- atomically $ flushTQueue queue
  case C.lines (C.concat (chunks ++ chunks')) of
    [csrf, _] -> do
      void $ send s $ C.concat [C.drop 8 csrf, C.pack "\n"]
      putStrLn $ C.unpack $ C.drop 8 csrf
      putStrLn "Authorized!"
      replState queue s
    _ -> csrfAuthState (chunks ++ chunks') queue s

-- TODO(#261): kgbotka-client does not detect closing the connection on the kgbotka side
main :: IO ()
main =
  runTCPClient "127.0.0.1" "6969" $ \s -> do
    putStrLn "Connected!"
    queue <- atomically newTQueue
    void $ forkIO $ readThread queue s
    csrfAuthState [] queue s

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

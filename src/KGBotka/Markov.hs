{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module KGBotka.Markov (addMarkovSentence, genMarkovSentence) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import System.Random

-- TODO(#46): Markov does not split models by twitch channels
-- TODO(#47): there is no way to retrain the model from the TwitchLog
-- TODO(#48): KGBotka does not response with Markov when mentioned
data Event
  = Begin
  | Word T.Text
  | End
  deriving (Eq, Read, Show, Ord)

instance ToField Event where
  toField = toField . T.pack . show

instance FromField Event where
  fromField = fmap (read . T.unpack) . fromField

addMarkovSentence :: Connection -> T.Text -> IO ()
addMarkovSentence conn sentence
  | T.length sentence >= 50 =
    mapM_ (addMarkovPair conn) $
    scanPairs $
    (\xs -> [Begin] <> xs <> [End]) $
    map Word $ T.words $ T.unwords $ T.words sentence
  | otherwise = return ()

addMarkovPair :: Connection -> (Event, Event) -> IO ()
addMarkovPair conn (event1, event2) = do
  n <-
    maybe (0 :: Int) fromOnly . listToMaybe <$>
    queryNamed
      conn
      "SELECT n FROM Markov WHERE event1 = :event1 AND event2 = :event2"
      [":event1" := event1, ":event2" := event2]
  executeNamed
    conn
    "INSERT INTO Markov (event1, event2, n) VALUES (:event1, :event2, :n)"
    [":event1" := event1, ":event2" := event2, ":n" := succ n]

nextMarkovEvent :: Connection -> Event -> MaybeT IO Event
nextMarkovEvent conn event1 = do
  bs <-
    lift $
    queryNamed
      conn
      "SELECT event2, n FROM Markov WHERE event1 = :event1"
      [":event1" := event1]
  let n :: Int
      n = foldl' (+) 0 $ map snd bs
  i <- lift $ randomRIO (0, n - 1)
  let a =
        dropWhile (\x -> snd x < i) $
        zip (map fst bs) $ scanl (+) 0 $ map snd bs
  case a of
    [] -> return End
    (event', _):_ -> return event'

seqMarkovEvents :: Event -> Event -> Connection -> IO [Event]
seqMarkovEvents begin end m
  | begin == end = return [end]
  | otherwise = do
    nxt <- runMaybeT $ nextMarkovEvent m begin
    case nxt of
      Just nxt' -> do
        rest <- seqMarkovEvents nxt' end m
        return (begin : rest)
      Nothing -> return []

scanPairs :: [a] -> [(a, a)]
scanPairs xs = zip xs $ tail xs

genMarkovSentence :: Connection -> IO T.Text
genMarkovSentence dbConn = do
  events <- seqMarkovEvents Begin End dbConn
  return $
    T.unwords $
    mapMaybe
      (\case
         Begin -> Nothing
         End -> Nothing
         Word x -> Just x)
      events

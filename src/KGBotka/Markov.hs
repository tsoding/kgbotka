{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Markov
  ( genMarkovSentence
  , isDiscordPing
  , MarkovCommand(..)
  , MarkovThreadParams(..)
  , markovThread
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.ToField
import KGBotka.Log
import KGBotka.Queue
import KGBotka.Sqlite
import System.Random
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String

-- TODO(#46): Markov does not split models by twitch channels
-- TODO(#47): there is no way to retrain the model from the TwitchLog
data Event
  = Begin
  | Word T.Text
  | End
  deriving (Eq, Read, Show, Ord)

instance ToField Event where
  toField = toField . T.pack . show

instance FromField Event where
  fromField = fmap (read . T.unpack) . fromField

isDiscordPing :: T.Text -> Bool
isDiscordPing text =
  either (const False) isJust $ do
    regex <- compile defaultCompOpt defaultExecOpt "<@!?[0-9]+>"
    execute regex (T.unpack text)

addMarkovSentence :: Sqlite.Connection -> T.Text -> IO ()
addMarkovSentence conn sentence
  | T.length sentence >= 50 =
    mapM_ (addMarkovPair conn) $
    scanPairs $
    (\xs -> [Begin] <> xs <> [End]) $
    map Word $
    filter (not . isDiscordPing) $ T.words $ T.unwords $ T.words sentence
  | otherwise = return ()

addMarkovPair :: Sqlite.Connection -> (Event, Event) -> IO ()
addMarkovPair conn (event1, event2) = do
  n <-
    maybe (0 :: Int) Sqlite.fromOnly . listToMaybe <$>
    Sqlite.queryNamed
      conn
      "SELECT n FROM Markov WHERE event1 = :event1 AND event2 = :event2"
      [":event1" := event1, ":event2" := event2]
  Sqlite.executeNamed
    conn
    "INSERT INTO Markov (event1, event2, n) VALUES (:event1, :event2, :n)"
    [":event1" := event1, ":event2" := event2, ":n" := succ n]

nextMarkovEvent :: Sqlite.Connection -> Event -> MaybeT IO Event
nextMarkovEvent conn event1 = do
  bs <-
    lift $
    Sqlite.queryNamed
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

seqMarkovEvents :: Event -> Event -> Sqlite.Connection -> IO [Event]
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

genMarkovSentence :: Sqlite.Connection -> IO T.Text
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

data MarkovCommand
  = NewSentence T.Text
  | Retrain
  | StopRetrain

data MarkovThreadParams = MarkovThreadParams
  { mtpSqliteConnection :: !(MVar Sqlite.Connection)
  , mtpLogQueue :: !(WriteQueue LogEntry)
  , mtpCmdQueue :: !(ReadQueue MarkovCommand)
  , mtpPageSize :: !Int
  , mtpRetrainProgress :: !(MVar (Maybe Int))
  }

withTransactionLogErrors ::
     MVar Sqlite.Connection
  -> WriteQueue LogEntry
  -> (Sqlite.Connection -> IO a)
  -> IO (Maybe a)
withTransactionLogErrors dbConn lqueue f =
  catch
    (withLockedTransaction dbConn $ fmap Just . f)
    (\e -> do
       logEntry lqueue $
         LogEntry "MARKOV" $ T.pack $ show (e :: Sqlite.SQLError)
       return Nothing)

markovThread :: MarkovThreadParams -> IO ()
markovThread mtp@MarkovThreadParams { mtpSqliteConnection = dbConn
                                    , mtpLogQueue = lqueue
                                    , mtpCmdQueue = cmdQueue
                                    , mtpRetrainProgress = retrainProgress
                                    , mtpPageSize = pageSize
                                    } = do
  threadDelay 10000 -- to prevent busy looping
  modifyMVar_ retrainProgress $ \case
    Just progress -> do
      cmd <- atomically $ tryReadQueue cmdQueue
      case cmd of
        Just Retrain -> do
          void $
            withTransactionLogErrors dbConn lqueue $ \conn ->
              Sqlite.executeNamed conn [sql|DELETE FROM Markov;|] []
          return $ Just 0
        Just StopRetrain -> return Nothing
        _
          -- TODO(#183): Markov retraining does not use Discord logs
         -> do
          processed <-
            withTransactionLogErrors dbConn lqueue $ \conn -> do
              messages <-
                Sqlite.queryNamed
                  conn
                  [sql| select message from TwitchLog
                      order by id limit :pageSize
                      offset :progress |]
                  [":pageSize" := pageSize, ":progress" := progress]
              traverse_ (addMarkovSentence conn . Sqlite.fromOnly) messages
              return $ length messages
          case processed of
            Just x
              | x > 0 -> return $ Just $ progress + x
            Just _ -> return Nothing
            Nothing -> return $ Just progress
    Nothing -> do
      cmd <- atomically $ tryReadQueue cmdQueue
      case cmd of
        Just (NewSentence text) -> do
          void $
            withTransactionLogErrors dbConn lqueue $ \conn ->
              addMarkovSentence conn text
          return Nothing
        Just Retrain -> do
          void $
            withTransactionLogErrors dbConn lqueue $ \conn ->
              Sqlite.executeNamed conn [sql|DELETE FROM Markov;|] []
          return $ Just 0
        Just StopRetrain -> return Nothing
        Nothing -> return Nothing
  markovThread mtp

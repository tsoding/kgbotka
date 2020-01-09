{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Friday
  ( submitVideo
  , FridayVideo(..)
  , nextVideo
  , currentRobin
  , Channel(..)
  , fakeFridayVideo
  ) where

import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import KGBotka.Roles (TwitchUserId(..))
import Data.Maybe
import Irc.Identifier (Identifier, idText, mkId)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Applicative
import Control.Monad

data FridayVideo = FridayVideo
  { fridayVideoId :: Int
  , fridayVideoSubText :: T.Text
  , fridayVideoSubTime :: UTCTime
  , fridayVideoAuthor :: TwitchUserId
  , fridayVideoChannel :: Channel
  }

fakeFridayVideo :: IO FridayVideo
fakeFridayVideo = do
  now <- getCurrentTime
  return $ FridayVideo 0 "" now (TwitchUserId "") (Channel $ mkId "")

instance FromRow FridayVideo where
  fromRow = FridayVideo <$> field <*> field <*> field <*> field <*> field

submitVideo :: Connection -> T.Text -> Channel -> TwitchUserId -> IO ()
submitVideo conn subText channel authorTwitchId =
  executeNamed
    conn
    "INSERT INTO FridayVideo \
    \(submissionText, submissionTime, authorTwitchId, channel) \
    \VALUES \
    \(:submissionText, datetime('now'), :authorTwitchId, :channel)"
    [ ":submissionText" := subText
    , ":authorTwitchId" := authorTwitchId
    , ":channel" := channel
    ]

newtype Channel = Channel Identifier

instance FromField Channel where
  fromField f = Channel . mkId <$> fromField f

instance ToField Channel where
    toField (Channel ident) = toField $ idText ident

currentRobin :: Connection -> Channel -> MaybeT IO TwitchUserId
currentRobin conn channel = current <|> first
  where
    current = do
      robin <-
        MaybeT
          (listToMaybe <$>
           queryNamed
             conn
             "SELECT robinTwitchId \
             \FROM FridayVideoRobin \
             \WHERE channel = :channel"
             [":channel" := channel])
      MaybeT $ return $ fromOnly $ robin
    first =
      MaybeT
        (listToMaybe <$>
         queryNamed
           conn
           "SELECT DISTINCT authorTwitchId a \
           \FROM FridayVideo \
           \WHERE watchedAt is NULL \
           \ORDER BY a \
           \ LIMIT 1"
           [])

watchVideo :: Connection -> Channel -> TwitchUserId -> MaybeT IO FridayVideo
watchVideo conn channel robin = do
  fridayVideo <-
    MaybeT
      (listToMaybe <$>
       queryNamed
         conn
         "SELECT id, submissionText, submissionTime, authorTwitchId, channel \
         \FROM FridayVideo \
         \WHERE authorTwitchId = :authorTwitchId \
         \AND channel = :channel \
         \AND watchedAt is NULL \
         \ORDER BY submissionTime LIMIT 1"
         [":authorTwitchId" := robin, ":channel" := channel])
  lift $
    executeNamed
      conn
      "UPDATE FridayVideo \
      \SET watchedAt = datetime('now') \
      \WHERE id = :fridayVideoId"
      [":fridayVideoId" := fridayVideoId fridayVideo]
  return fridayVideo

advanceRobin :: Connection -> Channel -> TwitchUserId -> IO ()
advanceRobin conn channel robin = do
  nextRobin <-
    listToMaybe <$>
    queryNamed
      conn
      "SELECT DISTINCT authorTwitchId a \
      \FROM FridayVideo \
      \WHERE authorTwitchId > :authorTwitchId \
      \AND channel = :channel \
      \AND watchedAt is NULL \
      \ORDER BY a LIMIT 1"
      [":authorTwitchId" := robin, ":channel" := channel]
  executeNamed
    conn
    "INSERT INTO FridayVideoRobin (robinTwitchId, channel) \
    \VALUES (:robinTwitchId, :channel)"
    [ ":robinTwitchId" := (join (fromOnly <$> nextRobin) :: Maybe TwitchUserId)
    , ":channel" := channel
    ]

nextVideo :: Connection -> Channel -> MaybeT IO FridayVideo
nextVideo conn channel = do
  robin <- currentRobin conn channel
  video <- watchVideo conn channel robin
  lift $ advanceRobin conn channel robin
  return video

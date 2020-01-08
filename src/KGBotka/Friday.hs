{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Friday
  ( submitVideo
  , FridayVideo(..)
  , nextVideo
  ) where

import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import KGBotka.Roles (TwitchUserId)

data FridayVideo = FridayVideo
  { fridayVideoId :: Int
  , fridayVideoSubText :: T.Text
  , fridayVideoSubTime :: UTCTime
  , fridayVideoAuthor :: TwitchUserId
  }

submitVideo :: Connection -> T.Text -> TwitchUserId -> IO ()
submitVideo conn subText authorTwitchId =
  executeNamed
    conn
    "INSERT INTO FridayVideo \
    \(submissionText, submissionTime, authorTwitchId) \
    \VALUES \
    \(:submissionText, datetime('now'), :authorTwitchId)"
    [":submissionText" := subText, ":authorTwitchId" := authorTwitchId]

-- TODO(#32): KGBotka.Friday.nextVideo is not implemented
nextVideo :: Connection -> IO (Maybe FridayVideo)
nextVideo _ = return Nothing

{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Friday
  ( submitVideo
  , FridayVideo(..)
  , nextVideo
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Maybe.Extra
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import KGBotka.TwitchAPI

data FridayVideo = FridayVideo
  { fridayVideoId :: Int
  , fridayVideoSubText :: T.Text
  , fridayVideoSubTime :: UTCTime
  , fridayVideoAuthorTwitchId :: TwitchUserId
  , fridayVideoAuthorTwitchName :: T.Text
  , fridayVideoWatchedAt :: Maybe UTCTime
  , fridayVideoChannel :: TwitchIrcChannel
  }

instance FromRow FridayVideo where
  fromRow =
    FridayVideo <$> field <*> field <*> field <*> field <*> field <*> field <*>
    field

submitVideo ::
     Connection -> T.Text -> TwitchIrcChannel -> TwitchUserId -> T.Text -> IO ()
submitVideo conn subText channel authorTwitchId authorTwitchName =
  executeNamed
    conn
    "INSERT INTO FridayVideo \
    \(submissionText, submissionTime, authorTwitchId, authorTwitchName, channel) \
    \VALUES \
    \(:submissionText, datetime('now'), :authorTwitchId, :authorTwitchName, :channel)"
    [ ":submissionText" := subText
    , ":authorTwitchId" := authorTwitchId
    , ":authorTwitchName" := authorTwitchName
    , ":channel" := channel
    ]

queueSlice :: Connection -> TwitchIrcChannel -> IO (M.Map TwitchUserId FridayVideo)
queueSlice conn channel =
  M.fromList . map (\x -> (fridayVideoAuthorTwitchId x, x)) <$>
  queryNamed
    conn
    "SELECT id, \
    \       submissionText, \
    \       min(submissionTime), \
    \       authorTwitchId, \
    \       authorTwitchName, \
    \       watchedAt, \
    \       channel \
    \FROM FridayVideo  \
    \WHERE watchedAt is NULL \
    \AND channel = :channel \
    \GROUP BY authorTwitchId"
    [":channel" := channel]

lastWatchedAuthor :: Connection -> TwitchIrcChannel -> MaybeT IO TwitchUserId
lastWatchedAuthor conn channel =
  MaybeT
    (listToMaybe <$>
     queryNamed
       conn
       "SELECT authorTwitchId FROM FridayVideo \
       \WHERE watchedAt IS NOT NULL \
       \  AND channel = :channel \
       \ORDER BY watchedAt DESC \
       \LIMIT 1"
       [":channel" := channel])

watchVideoById :: Connection -> Int -> IO ()
watchVideoById conn videoId =
  executeNamed
    conn
    "UPDATE FridayVideo \
    \SET watchedAt = datetime('now') \
    \WHERE id = :id"
    [":id" := videoId]

nextVideo :: Connection -> TwitchIrcChannel -> MaybeT IO FridayVideo
nextVideo conn channel = do
  author <- lastWatchedAuthor conn channel <|> return ""
  slice <- lift $ queueSlice conn channel
  video <-
    hoistMaybe (snd <$> M.lookupGT author slice) <|>
    hoistMaybe (snd <$> M.lookupGT "" slice)
  lift $ watchVideoById conn $ fridayVideoId video
  return video

{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Friday
  ( submitVideo
  , FridayVideo(..)
  , nextVideo
  , Channel(..)
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Maybe.Extra
import qualified Data.Map as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Irc.Identifier (Identifier, idText, mkId)
import KGBotka.Roles (TwitchUserId(..))

data FridayVideo = FridayVideo
  { fridayVideoId :: Int
  , fridayVideoSubText :: T.Text
  , fridayVideoSubTime :: UTCTime
  , fridayVideoAuthorTwitchId :: TwitchUserId
  , fridayVideoAuthorTwitchName :: T.Text
  , fridayVideoWatchedAt :: Maybe UTCTime
  , fridayVideoChannel :: Channel
  }

instance FromRow FridayVideo where
  fromRow =
    FridayVideo <$> field <*> field <*> field <*> field <*> field <*> field <*>
    field

submitVideo ::
     Connection -> T.Text -> Channel -> TwitchUserId -> T.Text -> IO ()
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

newtype Channel =
  Channel Identifier

instance IsString Channel where
  fromString = Channel . fromString

instance FromField Channel where
  fromField f = Channel . mkId <$> fromField f

instance ToField Channel where
  toField (Channel ident) = toField $ idText ident

queueSlice :: Connection -> Channel -> IO (M.Map TwitchUserId FridayVideo)
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

lastWatchedAuthor :: Connection -> Channel -> MaybeT IO TwitchUserId
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

nextVideo :: Connection -> Channel -> MaybeT IO FridayVideo
nextVideo conn channel = do
  author <- lastWatchedAuthor conn channel <|> return ""
  slice <- lift $ queueSlice conn channel
  video <-
    hoistMaybe (snd <$> M.lookupGT author slice) <|>
    hoistMaybe (snd <$> M.lookupGT "" slice)
  lift $ watchVideoById conn $ fridayVideoId video
  return video

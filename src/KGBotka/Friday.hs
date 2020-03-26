{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Friday
  ( submitVideo
  , FridayVideo(..)
  , nextVideo
  , fridayVideoAsMessage
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Extra
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import KGBotka.TwitchAPI

data FridayVideo = FridayVideo
  { fridayVideoId :: Int
  , fridayVideoSubText :: T.Text
  , fridayVideoSubTime :: UTCTime
  , fridayVideoAuthorTwitchId :: TwitchUserId
  , fridayVideoAuthorDisplayName :: T.Text
  , fridayVideoWatchedAt :: Maybe UTCTime
  }

instance FromRow FridayVideo where
  fromRow =
    FridayVideo <$> field <*> field <*> field <*> field <*> field <*> field

submitVideo ::
     Connection -> T.Text -> TwitchUserId -> T.Text -> IO ()
submitVideo conn subText authorTwitchId authorDisplayName =
  executeNamed
    conn
    [sql| INSERT INTO FridayVideo
          (submissionText, submissionTime, authorTwitchId, authorDisplayName)
          VALUES
          (:submissionText, datetime('now'), :authorTwitchId, :authorDisplayName) |]
    [ ":submissionText" := subText
    , ":authorTwitchId" := authorTwitchId
    , ":authorDisplayName" := authorDisplayName
    ]

queueSlice ::
     Connection -> IO (M.Map TwitchUserId FridayVideo)
queueSlice conn =
  M.fromList . map (\x -> (fridayVideoAuthorTwitchId x, x)) <$>
  queryNamed
    conn
    [sql|SELECT id,
                submissionText,
                min(submissionTime),
                authorTwitchId,
                authorDisplayName,
                watchedAt
         FROM FridayVideo
         WHERE watchedAt is NULL
         GROUP BY authorTwitchId|]
    []

lastWatchedAuthor :: Connection -> MaybeT IO TwitchUserId
lastWatchedAuthor conn =
  MaybeT
    (listToMaybe <$>
     queryNamed
       conn
       [sql| SELECT authorTwitchId FROM FridayVideo
             WHERE watchedAt IS NOT NULL
             ORDER BY watchedAt DESC
             LIMIT 1 |]
       [])

watchVideoById :: Connection -> Int -> IO ()
watchVideoById conn videoId =
  executeNamed
    conn
    [sql| UPDATE FridayVideo
          SET watchedAt = datetime('now')
          WHERE id = :id |]
    [":id" := videoId]

nextVideo :: Connection -> MaybeT IO FridayVideo
nextVideo conn = do
  author <- lastWatchedAuthor conn <|> return ""
  slice <- lift $ queueSlice conn
  video <-
    hoistMaybe (snd <$> M.lookupGT author slice) <|>
    hoistMaybe (snd <$> M.lookupGT "" slice)
  lift $ watchVideoById conn $ fridayVideoId video
  return video

fridayVideoAsMessage :: FridayVideo -> T.Text
fridayVideoAsMessage FridayVideo { fridayVideoSubText = subText
                                 , fridayVideoSubTime = subTime
                                 , fridayVideoAuthorDisplayName = authorDisplayName
                                 } =
  T.pack (show subTime) <> " <" <> authorDisplayName <> "> " <> subText

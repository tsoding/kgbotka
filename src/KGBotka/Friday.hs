{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Friday
  ( submitVideo
  , FridayVideo(..)
  , nextVideo
  , fridayVideoAsMessage
  , AuthorId(..)
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
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.QQ
import Data.String

newtype AuthorId = AuthorId T.Text deriving (Eq, Ord)

instance FromField AuthorId where
  fromField x = AuthorId <$> fromField x

instance FromRow AuthorId where
  fromRow = AuthorId <$> field

instance ToField AuthorId where
  toField (AuthorId x) = toField x

instance IsString AuthorId where
  fromString = AuthorId . T.pack

data FridayVideo = FridayVideo
  { fridayVideoId :: Int
  , fridayVideoSubText :: T.Text
  , fridayVideoSubTime :: UTCTime
  , fridayVideoAuthorId :: AuthorId
  , fridayVideoAuthorDisplayName :: T.Text
  , fridayVideoWatchedAt :: Maybe UTCTime
  }

instance FromRow FridayVideo where
  fromRow =
    FridayVideo <$> field <*> field <*> field <*> field <*> field <*> field

submitVideo ::
     Connection -> T.Text -> AuthorId -> T.Text -> IO ()
submitVideo conn subText authorId authorDisplayName =
  executeNamed
    conn
    [sql| INSERT INTO FridayVideo
          (submissionText, submissionTime, authorId, authorDisplayName)
          VALUES
          (:submissionText, datetime('now'), :authorId, :authorDisplayName) |]
    [ ":submissionText" := subText
    , ":authorId" := authorId
    , ":authorDisplayName" := authorDisplayName
    ]

queueSlice ::
     Connection -> IO (M.Map AuthorId FridayVideo)
queueSlice conn =
  M.fromList . map (\x -> (fridayVideoAuthorId x, x)) <$>
  queryNamed
    conn
    [sql|SELECT id,
                submissionText,
                min(submissionTime),
                authorId,
                authorDisplayName,
                watchedAt
         FROM FridayVideo
         WHERE watchedAt is NULL
         GROUP BY authorId|]
    []

lastWatchedAuthor :: Connection -> MaybeT IO AuthorId
lastWatchedAuthor conn =
  MaybeT
    (listToMaybe <$>
     queryNamed
       conn
       [sql| SELECT authorId FROM FridayVideo
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module KGBotka.Friday
  ( submitVideo
  , FridayVideo(..)
  , nextVideo
  , fridayVideoAsMessage
  , AuthorId(..)
  , fetchAllQueues
  , ytLinkId
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Extra
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.ToField
import qualified Text.Regex.Base.RegexLike as Regex
import qualified Text.Regex.TDFA.String as Regex
import Data.Array

newtype AuthorId =
  AuthorId T.Text
  deriving (Eq, Ord, Show)

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
  } deriving (Show)

instance FromRow FridayVideo where
  fromRow =
    FridayVideo <$> field <*> field <*> field <*> field <*> field <*> field

submitVideo :: Connection -> T.Text -> AuthorId -> T.Text -> IO ()
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

fetchQueueByAuthorId :: Connection -> AuthorId -> IO [FridayVideo]
fetchQueueByAuthorId dbConn authorId =
  queryNamed
    dbConn
    [sql|SELECT id,
                submissionText,
                submissionTime,
                authorId,
                authorDisplayName,
                watchedAt
         FROM FridayVideo
         WHERE watchedAt is NULL
         AND authorId = :authorId;|]
    [":authorId" := authorId]

fetchAllQueues :: Connection -> IO [[FridayVideo]]
fetchAllQueues dbConn = do
  authorIds <-
    queryNamed
      dbConn
      [sql|SELECT authorId
           FROM FridayVideo
           WHERE watchedAt is NULL
           GROUP BY authorId;|]
      []
  mapM (fetchQueueByAuthorId dbConn) authorIds

queueSlice :: Connection -> IO (M.Map AuthorId FridayVideo)
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

ytLinkRegex :: Either String Regex.Regex
ytLinkRegex =
  Regex.compile
    Regex.defaultCompOpt
    Regex.defaultExecOpt
    "https?:\\/\\/(www\\.)?youtu(be\\.com\\/watch\\?v=|\\.be\\/)([a-zA-Z0-9_-]+)"

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

-- | Extracts YouTube Video ID from the string
-- Results:
-- - `Right ytId` - extracted successfully
-- - `Left (Just failReason)` - extraction failed because of
--    the application's fault. The reason explained in `failReason`.
--    `failReason` should be logged and later investigated by the devs.
--    `failReason` should not be shown to the users.
-- - `Left Nothing` - extraction failed because of the user's fault.
--    Tell the user that their message does not contain any YouTube
--    links.
ytLinkId :: T.Text -> Either (Maybe String) T.Text
ytLinkId text = do
  regex <- mapLeft Just ytLinkRegex
  result <- mapLeft Just $ Regex.execute regex (T.unpack text)
  case result of
    Just matches ->
      case map (T.pack . flip Regex.extract (T.unpack text)) $ elems matches of
        [_, _, _, ytId] -> Right ytId
        _ ->
          Left $
          Just
            "Matches were not captured correctly. \
            \Most likely somebody changed the YouTube \
            \link regular expression (`ytLinkRegex`) and didn't \
            \update `ytLinkId` function to extract capture \
            \groups correctly. ( =_=)"
    Nothing -> Left Nothing

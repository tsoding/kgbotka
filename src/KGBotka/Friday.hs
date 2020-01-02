module KGBotka.Friday (submitVideo) where

import Data.Time
import qualified Data.Text as T
import Database.SQLite.Simple
import KGBotka.Roles (TwitchUserId)

submitVideo :: Connection
            -> T.Text
            -> UTCTime
            -> TwitchUserId
            -> IO ()
submitVideo conn subText subTime authorTwitchId = do
  executeNamed
    conn
    "INSERT INTO FridayVideo \
    \(submissionText, submissionTime, authorTwitchId) \
    \VALUES \
    \(:submissionText, :submissionTime, :authorTwitchId)"
    [ ":submissionText" := subText
    , ":submissionTime" := subTime
    , ":authorTwitchId" := authorTwitchId
    ]

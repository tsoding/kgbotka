{-# LANGUAGE OverloadedStrings #-}
module KGBotka.Friday (submitVideo) where

import qualified Data.Text as T
import Database.SQLite.Simple
import KGBotka.Roles (TwitchUserId)

submitVideo :: Connection
            -> T.Text
            -> TwitchUserId
            -> IO ()
submitVideo conn subText authorTwitchId = do
  executeNamed
    conn
    "INSERT INTO FridayVideo \
    \(submissionText, submissionTime, authorTwitchId) \
    \VALUES \
    \(:submissionText, datetime('now'), :authorTwitchId)"
    [ ":submissionText" := subText
    , ":authorTwitchId" := authorTwitchId
    ]

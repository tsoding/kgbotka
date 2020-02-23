{-# LANGUAGE OverloadedStrings #-}

module KGBotka.TwitchLog
  ( logMessage
  ) where

import qualified Data.Text as T
import Database.SQLite.Simple
import KGBotka.Roles
import KGBotka.TwitchAPI

logMessage ::
     Connection
  -> TwitchIrcChannel
  -> TwitchUserId
  -> T.Text
  -> Maybe T.Text
  -> [TwitchRole]
  -> [TwitchBadgeRole]
  -> T.Text
  -> IO ()
logMessage conn channel senderTwitchId senderTwitchName senderTwitchDisplayName senderTwitchRoles senderTwitchBadgeRoles message =
  executeNamed
    conn
    "INSERT INTO TwitchLog ( \
    \  channel, \
    \  senderTwitchId, \
    \  senderTwitchName, \
    \  senderTwitchDisplayName, \
    \  senderTwitchRoles, \
    \  senderTwitchBadgeRoles, \
    \  message \
    \) VALUES ( \
    \  :channel, \
    \  :senderTwitchId, \
    \  :senderTwitchName, \
    \  :senderTwitchDisplayName, \
    \  :senderTwitchRoles, \
    \  :senderTwitchBadgeRoles, \
    \  :message \
    \)"
    [ ":channel" := channel
    , ":senderTwitchId" := senderTwitchId
    , ":senderTwitchName" := senderTwitchName
    , ":senderTwitchDisplayName" := senderTwitchDisplayName
    -- NOTE: Roles and BadgeRoles in this table are supposed to be
    -- informative. Basically they are just a reflection of what they
    -- were at the time. Should not be generally used for any
    -- authentication or decision making.
    , ":senderTwitchRoles" := show senderTwitchRoles
    , ":senderTwitchBadgeRoles" := show senderTwitchBadgeRoles
    , ":message" := message
    ]

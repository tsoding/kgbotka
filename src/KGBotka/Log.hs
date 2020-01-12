{-# LANGUAGE OverloadedStrings #-}
module KGBotka.Log (logMessage) where

import KGBotka.TwitchAPI
import qualified Data.Text as T
import Database.SQLite.Simple
import KGBotka.Roles

logMessage ::
     Connection
  -> TwitchIrcChannel
  -> Maybe TwitchUserId
  -> T.Text
  -> Maybe T.Text
  -> [TwitchRole]
  -> [TwitchBadgeRole]
  -> T.Text
  -> IO ()
logMessage conn channel senderTwitchId senderTwitchName senderTwitchDisplayName senderTwitchRoles senderTwitchBadgeRoles message = do
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
    -- FIXME: roles are not serialized properly?
    , ":senderTwitchRoles" := show senderTwitchRoles
    , ":senderTwitchBadgeRoles" := show senderTwitchBadgeRoles
    , ":message" := message
    ]

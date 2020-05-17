{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module KGBotka.DiscordLog where

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Discord.Types

logMessage ::
     Connection
  -> Maybe GuildId
  -> ChannelId
  -> UserId
  -> T.Text
  -> T.Text
  -> IO ()
logMessage dbConn guild channel user displayName message =
  executeNamed
    dbConn
    [sql|INSERT INTO DiscordLog (
           guildId,
           channelId,
           senderDiscordId,
           senderDiscordDisplayName,
           message
         ) VALUES (
           :guildId,
           :channelId,
           :senderDiscordId,
           :senderDiscordDisplayName,
           :message
         ) |]
    [ ":guildId" := T.pack (show guild)
    , ":channelId" := T.pack (show channel)
    , ":senderDiscordId" := T.pack (show user)
    , ":senderDiscordDisplayName" := displayName
    , ":message" := message
    ]

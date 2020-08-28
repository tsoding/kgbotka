{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module KGBotka.JoinedTwitchChannels
  ( joinedChannels
  , registerJoinedChannel
  , unregisterJoinedChannel
  , callPrefixOfJoinedChannel
  , setPrefixOfJoinedChannel
  ) where

import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple.QQ
import KGBotka.TwitchAPI

data JoinedTwitchChannel = JoinedTwitchChannel
    { joinedChannelTwitchChannel :: TwitchIrcChannel
    , joinedChannelCallPrefix :: T.Text
    }

instance FromRow JoinedTwitchChannel where
    fromRow = JoinedTwitchChannel <$> field <*> field

joinedChannels :: Sqlite.Connection -> IO [TwitchIrcChannel]
joinedChannels dbConn = do
  channels <-
    Sqlite.queryNamed dbConn [sql|SELECT * FROM JoinedTwitchChannels;|] []
  return $ map joinedChannelTwitchChannel channels

registerJoinedChannel :: Sqlite.Connection -> TwitchIrcChannel -> IO ()
registerJoinedChannel dbConn channel =
  Sqlite.executeNamed
    dbConn
    [sql|INSERT INTO JoinedTwitchChannels (name)
         VALUES (:channel);|]
    [":channel" := channel]

callPrefixOfJoinedChannel :: Sqlite.Connection -> TwitchIrcChannel -> IO T.Text
callPrefixOfJoinedChannel dbConn channel =
    joinedChannelCallPrefix . head <$>
                            Sqlite.queryNamed dbConn
                                      [sql|SELECT * FROM JoinedTwitchChannels WHERE name = :channel;|]
                                      [":channel" := channel]

setPrefixOfJoinedChannel :: Sqlite.Connection -> TwitchIrcChannel -> T.Text -> IO ()
setPrefixOfJoinedChannel dbConn channel prefix =
  Sqlite.executeNamed
    dbConn
    [sql|UPDATE JoinedTwitchChannels SET channelCommandPrefix = :pref WHERE name = :channel;|]
    [":channel" := channel, ":pref" := prefix]

unregisterJoinedChannel :: Sqlite.Connection -> TwitchIrcChannel -> IO ()
unregisterJoinedChannel dbConn channel =
  Sqlite.executeNamed
    dbConn
    [sql|DELETE FROM JoinedTwitchChannels
         WHERE name = :channel|]
    [":channel" := channel]

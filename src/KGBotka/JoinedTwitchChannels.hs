{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module KGBotka.JoinedTwitchChannels
  ( joinedChannels
  , registerJoinedChannel
  , unregisterJoinedChannel
  ) where

import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple.QQ
import KGBotka.TwitchAPI

joinedChannels :: Sqlite.Connection -> IO [TwitchIrcChannel]
joinedChannels dbConn = do
  channels <-
    Sqlite.queryNamed dbConn [sql|SELECT * FROM JoinedTwitchChannels;|] []
  return $ map Sqlite.fromOnly channels

registerJoinedChannel :: Sqlite.Connection -> TwitchIrcChannel -> IO ()
registerJoinedChannel dbConn channel =
  Sqlite.executeNamed
    dbConn
    [sql|INSERT INTO JoinedTwitchChannels (name)
         VALUES (:channel)|]
    [":channel" := channel]

unregisterJoinedChannel :: Sqlite.Connection -> TwitchIrcChannel -> IO ()
unregisterJoinedChannel dbConn channel =
  Sqlite.executeNamed
    dbConn
    [sql|DELETE FROM JoinedTwitchChannels
         WHERE name = :channel|]
    [":channel" := channel]

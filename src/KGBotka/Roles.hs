{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module KGBotka.Roles
  ( assTwitchRoleToUser
  , delTwitchRoleFromUser
  , getTwitchUserRoles
  , getTwitchRoleByName
  , TwitchRole(..)
  , TwitchBadgeRole(..)
  , addTwitchRole
  , listTwitchRoles
  ) where

import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import KGBotka.TwitchAPI
import KGBotka.Sqlite

data TwitchBadgeRole
  = TwitchSub
  | TwitchVip
  | TwitchBroadcaster
  | TwitchMod
  deriving (Eq, Show)

data TwitchRole = TwitchRole
  { twitchRoleId :: Int
  , twitchRoleName :: T.Text
  } deriving (Show)

instance FromRow TwitchRole where
  fromRow = TwitchRole <$> field <*> field

assTwitchRoleToUser :: ProvidesDatabase s => s -> Int -> TwitchUserId -> IO ()
assTwitchRoleToUser (getSqliteConnection -> conn) roleId' userId' =
  executeNamed
    conn
    "INSERT INTO TwitchUserRoles (userId, roleId) \
    \VALUES (:userId, :roleId);"
    [":userId" := userId', ":roleId" := roleId']

delTwitchRoleFromUser :: Connection -> Int -> TwitchUserId -> IO ()
delTwitchRoleFromUser = undefined

getTwitchUserRoles :: Connection -> TwitchUserId -> IO [TwitchRole]
getTwitchUserRoles conn userId = queryNamed conn queryText [":userId" := userId]
  where
    queryText =
      "SELECT ur.roleId, r.name \
      \FROM TwitchUserRoles ur \
      \INNER JOIN TwitchRoles r \
      \ON ur.roleId = r.id \
      \WHERE ur.userId = :userId;"

getTwitchRoleByName ::
     ProvidesDatabase s => s -> T.Text -> IO (Maybe TwitchRole)
getTwitchRoleByName (getSqliteConnection -> conn) name =
  listToMaybe <$>
  queryNamed
    conn
    "SELECT * FROM TwitchRoles \
    \WHERE name = :roleName;"
    [":roleName" := name]

addTwitchRole :: ProvidesDatabase s => s -> T.Text -> IO ()
addTwitchRole (getSqliteConnection -> dbConn) name =
  executeNamed
    dbConn
    [sql|INSERT INTO TwitchRoles (name) VALUES (:name)|]
    [":name" := name]

listTwitchRoles :: ProvidesDatabase s => s -> IO [TwitchRole]
listTwitchRoles (getSqliteConnection -> dbConn) =
  queryNamed dbConn [sql|SELECT id, name FROM TwitchRoles |] []

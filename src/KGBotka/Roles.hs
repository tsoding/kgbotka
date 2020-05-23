{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module KGBotka.Roles
  ( assTwitchRoleToUser
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
import Data.Int

data TwitchBadgeRole
  = TwitchSub
  | TwitchVip
  | TwitchBroadcaster
  | TwitchMod
  | TwitchFounder
  deriving (Eq, Show)

data TwitchRole = TwitchRole
  { twitchRoleId :: Int64
  , twitchRoleName :: T.Text
  } deriving (Show)

instance FromRow TwitchRole where
  fromRow = TwitchRole <$> field <*> field

assTwitchRoleToUser :: Connection -> Int64 -> TwitchUserId -> IO ()
assTwitchRoleToUser conn roleId' userId' =
  executeNamed
    conn
    "INSERT INTO TwitchUserRoles (userId, roleId) \
    \VALUES (:userId, :roleId);"
    [":userId" := userId', ":roleId" := roleId']

getTwitchUserRoles :: Connection -> TwitchUserId -> IO [TwitchRole]
getTwitchUserRoles conn userId = queryNamed conn queryText [":userId" := userId]
  where
    queryText =
      "SELECT ur.roleId, r.name \
      \FROM TwitchUserRoles ur \
      \INNER JOIN TwitchRoles r \
      \ON ur.roleId = r.id \
      \WHERE ur.userId = :userId;"

getTwitchRoleByName :: Connection -> T.Text -> IO (Maybe TwitchRole)
getTwitchRoleByName conn name =
  listToMaybe <$>
  queryNamed
    conn
    "SELECT * FROM TwitchRoles \
    \WHERE name = :roleName;"
    [":roleName" := name]

addTwitchRole :: Connection -> T.Text -> IO Int64
addTwitchRole dbConn name = do
  executeNamed
    dbConn
    [sql|INSERT INTO TwitchRoles (name) VALUES (:name)|]
    [":name" := name]
  lastInsertRowId dbConn

listTwitchRoles :: Connection -> IO [TwitchRole]
listTwitchRoles dbConn =
  queryNamed dbConn [sql|SELECT id, name FROM TwitchRoles |] []

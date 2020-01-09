{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Roles
  ( assTwitchRoleToUser
  , delTwitchRoleFromUser
  , getTwitchUserRoles
  , getTwitchRoleByName
  , TwitchRole(..)
  , TwitchUserId(..)
  ) where

import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

newtype TwitchUserId = TwitchUserId
  { twitchUserId :: T.Text
  } deriving (Show, Eq)

data TwitchRole = TwitchRole
  { twitchRoleId :: Int
  , twitchRoleName :: T.Text
  } deriving (Show)

instance ToField TwitchUserId where
  toField = toField . twitchUserId

instance FromField TwitchUserId where
  fromField f = TwitchUserId <$> fromField f

instance FromRow TwitchUserId where
  fromRow = TwitchUserId <$> field

instance FromRow TwitchRole where
  fromRow = TwitchRole <$> field <*> field

assTwitchRoleToUser :: Connection -> Int -> TwitchUserId -> IO ()
assTwitchRoleToUser conn roleId' userId' =
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

getTwitchRoleByName :: Connection -> T.Text -> IO (Maybe TwitchRole)
getTwitchRoleByName conn name =
  listToMaybe <$>
  queryNamed
    conn
    "SELECT * FROM TwitchRoles \
    \WHERE name = :roleName;"
    [":roleName" := name]

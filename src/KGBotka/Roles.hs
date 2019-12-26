{-# LANGUAGE OverloadedStrings #-}
module KGBotka.Roles
  ( addRoleToUser
  , delRoleFromUser
  , getUserRoles
  , getRoleByName
  , Role(..)
  , TwitchUserId(..)
  ) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Maybe

newtype TwitchUserId = TwitchUserId
  { twitchUserId :: T.Text
  } deriving (Show, Eq)

data Role = Role
  { roleId :: Int
  , roleName :: T.Text
  } deriving (Show)

instance ToField TwitchUserId where
  toField = toField . twitchUserId

instance FromRow Role where
  fromRow = Role <$> field <*> field

addRoleToUser :: Connection -> Int -> TwitchUserId -> IO ()
addRoleToUser conn roleId' userId' = do
  executeNamed
    conn
    "INSERT INTO TwitchUserRoles (userId, roleId) \
    \VALUES (:userId, :roleId);"
    [":userId" := userId', ":roleId" := roleId']

delRoleFromUser :: Connection -> Int -> TwitchUserId -> IO ()
delRoleFromUser = undefined

getUserRoles :: Connection -> TwitchUserId -> IO [Role]
getUserRoles conn userId = do
  queryNamed conn queryText [":userId" := userId]
  where
    queryText =
      "SELECT ur.roleId, r.name \
      \FROM TwitchUserRoles ur \
      \INNER JOIN TwitchRoles r \
      \ON ur.roleId = r.id \
      \WHERE ur.userId = :userId;"

getRoleByName :: Connection -> T.Text -> IO (Maybe Role)
getRoleByName conn name =
  listToMaybe <$>
  queryNamed
    conn
    "SELECT * FROM TwitchRoles \
    \WHERE name = :roleName;"
    [":roleName" := name]

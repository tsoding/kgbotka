module KGBotka.Roles
  ( addRoleToUser
  , delRoleFromUser
  , getUserRoles
  , Role(..)
  ) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

newtype TwitchUserId = TwitchUserId
  { twitchUserId :: Int
  } deriving (Show, Eq)

data Role = Role
  { roleId :: Int
  , roleName :: T.Text
  } deriving (Show)

instance ToField TwitchUserId where
  toField = toField . twitchUserId

instance FromRow Role where
  fromRow = Role <$> field <*> field

addRoleToUser :: Connection -> TwitchUserId -> Int -> IO ()
addRoleToUser = undefined

delRoleFromUser :: Connection -> TwitchUserId -> Int -> IO ()
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

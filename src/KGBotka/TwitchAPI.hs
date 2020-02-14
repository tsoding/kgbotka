{-# LANGUAGE OverloadedStrings #-}

module KGBotka.TwitchAPI
  ( TwitchUser(..)
  , TwitchRes(..)
  , TwitchUserId(..)
  , JsonResponse(..)
  , getUsersByLogins
  , TwitchIrcChannel(..)
  , twitchIrcChannelText
  , mkTwitchIrcChannel
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Irc.Identifier (Identifier, idText, mkId)
import KGBotka.Http
import Network.HTTP.Client

newtype TwitchUserId =
  TwitchUserId T.Text
  deriving (Show, Eq, Ord)

instance IsString TwitchUserId where
  fromString = TwitchUserId . fromString

instance ToField TwitchUserId where
  toField (TwitchUserId userId) = toField userId

instance FromField TwitchUserId where
  fromField f = TwitchUserId <$> fromField f

instance FromRow TwitchUserId where
  fromRow = TwitchUserId <$> field

instance FromJSON TwitchUserId where
  parseJSON v = TwitchUserId <$> parseJSON v

newtype TwitchIrcChannel =
  TwitchIrcChannel Identifier
  deriving (Ord, Eq)

twitchIrcChannelText :: TwitchIrcChannel -> T.Text
twitchIrcChannelText (TwitchIrcChannel ident) = idText ident

mkTwitchIrcChannel :: T.Text -> TwitchIrcChannel
mkTwitchIrcChannel = TwitchIrcChannel . mkId

instance IsString TwitchIrcChannel where
  fromString = TwitchIrcChannel . fromString

instance FromField TwitchIrcChannel where
  fromField f = TwitchIrcChannel . mkId <$> fromField f

instance ToField TwitchIrcChannel where
  toField (TwitchIrcChannel ident) = toField $ idText ident

data TwitchUser = TwitchUser
  { twitchUserId :: TwitchUserId
  , twitchUserLogin :: T.Text
  } deriving (Show)

newtype TwitchRes a = TwitchRes
  { twitchResData :: a
  }

instance FromJSON a => FromJSON (TwitchRes a) where
  parseJSON (Object v) = TwitchRes <$> v .: "data"
  parseJSON invalid = typeMismatch "TwitchRes" invalid

instance FromJSON TwitchUser where
  parseJSON (Object v) = TwitchUser <$> v .: "id" <*> v .: "login"
  parseJSON invalid = typeMismatch "TwitchUser" invalid

getUsersByLogins ::
     Manager -> T.Text -> [T.Text] -> IO (JsonResponse [TwitchUser])
getUsersByLogins manager clientId users = do
  let url =
        "https://api.twitch.tv/helix/users?" <>
        T.concat (intersperse "&" $ map ("login=" <>) users)
  request <- parseRequest $ T.unpack url
  response <-
    httpJson manager $
    request
      { requestHeaders =
          ("Client-ID", encodeUtf8 clientId) : requestHeaders request
      }
  return (twitchResData <$> response)

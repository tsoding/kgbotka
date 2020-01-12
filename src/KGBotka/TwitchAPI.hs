{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module KGBotka.TwitchAPI
  ( TwitchUser(..)
  , TwitchRes(..)
  , TwitchUserId(..)
  , JsonResponse(..)
  , getUsersByLogins
  ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import Data.List
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Client
import Data.String
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

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

newtype JsonResponse a = JsonResponse
  { unwrapJsonResponse :: Response (Either String a)
  } deriving (Functor)

httpJson :: FromJSON a => Manager -> Request -> IO (JsonResponse a)
httpJson manager request = do
  response <- httpLbs request manager
  putStrLn $ T.unpack $ decodeUtf8 $ BS.toStrict $ responseBody response
  return $ JsonResponse (eitherDecode <$> response)

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

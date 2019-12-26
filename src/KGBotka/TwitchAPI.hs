{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
module KGBotka.TwitchAPI
  ( TwitchUser(..)
  , TwitchRes(..)
  , JsonResponse(..)
  , getUsersByLogins
  ) where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Client
import Data.List
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as BS

data TwitchUser = TwitchUser
  { userId :: T.Text
  , userLogin :: T.Text
  } deriving (Show)

data TwitchRes a = TwitchRes { twitchResData :: a }

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
        (T.concat $ intersperse "&" $ map ("login=" <>) users)
  request <- parseRequest $ T.unpack url
  response <- httpJson manager $
    request
      { requestHeaders =
          ("Client-ID", encodeUtf8 clientId) : requestHeaders request
      }
  return (twitchResData <$> response)

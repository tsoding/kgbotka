{-# LANGUAGE OverloadedStrings #-}

module KGBotka.TwitchAPI
  ( TwitchUser(..)
  , TwitchRes(..)
  , TwitchUserId(..)
  , getUsersByLogins
  , TwitchIrcChannel(..)
  , twitchIrcChannelText
  , mkTwitchIrcChannel
  , getStreamByLogin
  , twitchIrcChannelName
  , TwitchStream(..)
  ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Irc.Identifier (Identifier, idText, mkId)
import KGBotka.Config
import Network.HTTP.Client
import Network.HTTP.Types.Status (Status(statusCode))

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

twitchIrcChannelName :: TwitchIrcChannel -> T.Text
twitchIrcChannelName channel =
  case T.uncons channelText of
    Just ('#', channelName) -> channelName
    _ -> channelText
  where
    channelText = twitchIrcChannelText channel

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

data TwitchErr = TwitchErr
  { twitchErrMessage :: T.Text
  , twitchErrStatus :: Int
  , twitchErrError :: T.Text
  } deriving (Show)

instance FromJSON TwitchErr where
  parseJSON (Object v) =
    TwitchErr <$> v .: "message" <*> v .: "status" <*> v .: "error"
  parseJSON invalid = typeMismatch "TwitchErr" invalid

newtype TwitchRes a = TwitchRes
  { twitchResData :: a
  }

instance FromJSON a => FromJSON (TwitchRes a) where
  parseJSON (Object v) = TwitchRes <$> v .: "data"
  parseJSON invalid = typeMismatch "TwitchRes" invalid

data TwitchStream = TwitchStream
  { tsStartedAt :: UTCTime
  , tsTitle :: T.Text
  } deriving (Eq, Show)

instance FromJSON TwitchStream where
  parseJSON (Object obj) =
    TwitchStream <$> obj .: "started_at" <*> obj .: "title"
  parseJSON invalid = typeMismatch "TwitchStream" invalid

instance FromJSON TwitchUser where
  parseJSON (Object v) = TwitchUser <$> v .: "id" <*> v .: "login"
  parseJSON invalid = typeMismatch "TwitchUser" invalid

extractTwitchResponse ::
     FromJSON a => Response B.ByteString -> Either TwitchErr a
extractTwitchResponse response
  | status >= 400 =
    case eitherDecode body of
      Right err -> Left err
      Left err -> Left $ TwitchErr (T.pack err) 413 "Parsing error"
  | otherwise =
    case eitherDecode body of
      Right res -> Right (twitchResData res)
      Left err -> Left $ TwitchErr (T.pack err) 413 "Parsing error"
  where
    body = responseBody response
    status = statusCode $ responseStatus response

getUsersByLogins ::
     Manager -> ConfigTwitch -> [T.Text] -> IO (Either TwitchErr [TwitchUser])
getUsersByLogins manager ConfigTwitch { configTwitchClientId = clientId
                                      , configTwitchToken = token
                                      } users
  -- TODO(#222): Consider using network-uri for constructing uri-s
  --
  -- Grep for @uri
 = do
  let url =
        "https://api.twitch.tv/helix/users?" <>
        T.concat (intersperse "&" $ map ("login=" <>) users)
  request <- parseRequest $ T.unpack url
  response <-
    httpLbs
      request
        { requestHeaders =
            ("Authorization", encodeUtf8 ("Bearer " <> token)) :
            ("Client-ID", encodeUtf8 clientId) : requestHeaders request
        }
      manager
  return $ extractTwitchResponse response

getStreamByLogin ::
     Manager
  -> ConfigTwitch
  -> T.Text
  -> IO (Either TwitchErr (Maybe TwitchStream))
getStreamByLogin manager ConfigTwitch { configTwitchClientId = clientId
                                      , configTwitchToken = token
                                      } login
  -- @uri
 = do
  let url = "https://api.twitch.tv/helix/streams?user_login=" <> T.unpack login
  request <- parseRequest url
  response <-
    httpLbs
      request
        { requestHeaders =
            ("Authorization", encodeUtf8 ("Bearer " <> token)) :
            ("Client-ID", encodeUtf8 clientId) : requestHeaders request
        }
      manager
  return $ listToMaybe <$> extractTwitchResponse response
-- TODO(#216): convenient mechanism of settings up the Twitch token

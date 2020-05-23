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
import Data.Functor.Compose
import Data.List
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Irc.Identifier (Identifier, idText, mkId)
import KGBotka.Http
import Network.HTTP.Client
import KGBotka.Config

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

getUsersByLogins ::
     Manager
  -> ConfigTwitch
  -> [T.Text]
  -> IO (Response (Either String [TwitchUser]))
getUsersByLogins manager ConfigTwitch {configTwitchClientId = clientId} users = do
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
  return $ getCompose (twitchResData <$> Compose response)

getStreamByLogin ::
     Manager -> T.Text -> T.Text -> IO (Either String (Maybe TwitchStream))
getStreamByLogin manager clientId login = do
  let url = "https://api.twitch.tv/helix/streams?user_login=" <> T.unpack login
  request <- parseRequest url
  response <-
    httpJson
      manager
      request
        { requestHeaders =
            ("Client-ID", encodeUtf8 clientId) : requestHeaders request
        }
  return (listToMaybe . twitchResData <$> responseBody response)

-- TODO(#216): convenient mechanism of settings up the Twitch token
-- TODO: getStreamByLogin and getUsersByLogins should also send the Authorization header

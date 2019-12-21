{-# LANGUAGE OverloadedStrings #-}
module KGBotka.TwitchAPI
  ( TwitchUser(..)
  , TwitchRes(..)
  , getUsersByLogins
  ) where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Client
import Data.List
import Control.Monad.Trans.Except
import Control.Exception

data TwitchUser = TwitchUser
  { userId :: Int
  , userLogin :: T.Text
  } deriving (Show)

data TwitchRes a = TwitchRes { twitchResData :: a }

instance FromJSON a => FromJSON (TwitchRes a) where
  parseJSON (Object v) = TwitchRes <$> v .: "data"
  parseJSON invalid = typeMismatch "TwitchRes" invalid

instance FromJSON TwitchUser where
  parseJSON (Object v) = TwitchUser <$> v .: "id" <*> v .: "login"
  parseJSON invalid = typeMismatch "TwitchUser" invalid

getUsersByLogins ::
     Manager -> T.Text -> [T.Text] -> ExceptT SomeException IO [TwitchUser]
getUsersByLogins manager clientId users = do
  let url =
        "https://api.twitch.tv/helix/users" <>
        (T.concat $ intersperse "&" $ map ("login=" <>) users)
  request <- ExceptT $ return $ parseRequest $ T.unpack url
  response <- ExceptT (return <$> httpLbs request manager)
  ExceptT $ return (twitchResData <$> eitherDecode (responseBody response))

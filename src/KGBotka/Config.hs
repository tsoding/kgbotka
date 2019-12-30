{-# LANGUAGE OverloadedStrings #-}
module KGBotka.Config where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

data ConfigTwitch = ConfigTwitch
  { configTwitchAccount :: T.Text
  , configTwitchToken :: T.Text
  , configTwitchClientId :: T.Text
  } deriving (Eq)

instance FromJSON ConfigTwitch where
  parseJSON (Object v) =
    ConfigTwitch <$> v .: "account" <*> v .: "token" <*> v .: "clientId"
  parseJSON invalid = typeMismatch "Config" invalid

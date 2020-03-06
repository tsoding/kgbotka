{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Config where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

data Config = Config
  { configTwitch :: !(Maybe ConfigTwitch)
  , configDiscord :: !(Maybe ConfigDiscord)
  } deriving (Eq)

data ConfigTwitch = ConfigTwitch
  { configTwitchAccount :: !T.Text
  , configTwitchToken :: !T.Text
  , configTwitchClientId :: !T.Text
  } deriving (Eq)

newtype ConfigDiscord = ConfigDiscord
  { configDiscordToken :: T.Text
  } deriving (Eq)

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .:? "twitch" <*> v .:? "discord"
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON ConfigTwitch where
  parseJSON (Object v) =
    ConfigTwitch <$> v .: "account" <*> v .: "token" <*> v .: "clientId"
  parseJSON invalid = typeMismatch "ConfigTwitch" invalid

instance FromJSON ConfigDiscord where
  parseJSON (Object v) = ConfigDiscord <$> v .: "token"
  parseJSON invalid = typeMismatch "DiscordTwitch" invalid

module KGBotka.Http
  ( httpJson
  , ProvidesHttpManager(..)
  ) where

import Data.Aeson
import Network.HTTP.Client

httpJson :: FromJSON a => Manager -> Request -> IO (Response (Either String a))
httpJson manager request = do
  response <- httpLbs request manager
  return (eitherDecode <$> response)

class ProvidesHttpManager phm where
  httpManager :: phm -> Manager

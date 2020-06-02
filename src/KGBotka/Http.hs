module KGBotka.Http
  ( httpJson
  ) where

import Data.Aeson
import Network.HTTP.Client

httpJson :: FromJSON a => Manager -> Request -> IO (Response (Either String a))
httpJson manager request = do
  response <- httpLbs request manager
  return (eitherDecode <$> response)

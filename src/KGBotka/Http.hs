{-# LANGUAGE DeriveFunctor #-}

module KGBotka.Http (JsonResponse(..), httpJson) where

import Network.HTTP.Client
import Data.Aeson

newtype JsonResponse a = JsonResponse
  { unwrapJsonResponse :: Response (Either String a)
  } deriving (Functor)

httpJson :: FromJSON a => Manager -> Request -> IO (JsonResponse a)
httpJson manager request = do
  response <- httpLbs request manager
  return $ JsonResponse (eitherDecode <$> response)

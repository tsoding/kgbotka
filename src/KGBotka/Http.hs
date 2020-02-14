{-# LANGUAGE DeriveFunctor #-}

module KGBotka.Http
  ( JsonResponse(..)
  , httpJson
  ) where

import Data.Aeson
import Network.HTTP.Client

newtype JsonResponse a = JsonResponse
  { unwrapJsonResponse :: Response (Either String a)
  } deriving (Functor)

httpJson :: FromJSON a => Manager -> Request -> IO (JsonResponse a)
httpJson manager request = do
  response <- httpLbs request manager
  return $ JsonResponse (eitherDecode <$> response)

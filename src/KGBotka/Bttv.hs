{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module KGBotka.Bttv (updateBttvEmotes) where

import Network.HTTP.Client
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Data.Foldable
import Data.Aeson
import Data.Aeson.Types
import KGBotka.Http
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.Extra
import Network.URI

data BttvEmote = BttvEmote
  { bttvEmoteName :: T.Text
  , bttvEmoteImageUrl :: T.Text
  }

newtype BttvRes = BttvRes
  { bttvResEmotes :: [BttvEmote]
  }

instance FromJSON BttvRes where
  parseJSON (Object v) = BttvRes <$> v .: "emotes"
  parseJSON invalid = typeMismatch "BttvRes" invalid

instance FromJSON BttvEmote where
  parseJSON (Object v) = BttvEmote <$> code <*> url
    where
      code = v .: "code"
      url =
        (\id' -> "https://cdn.betterttv.net/emote/" <> id' <> "/3x") <$>
        (v .: "id")
  parseJSON invalid = typeMismatch "BttvEmote" invalid

queryBttvEmotes :: Manager -> Maybe T.Text -> ExceptT String IO [BttvEmote]
queryBttvEmotes manager Nothing = do
  request <- parseRequest "https://api.betterttv.net/2/emotes"
  response <- lift $ httpJson manager request
  hoistEither $ fmap bttvResEmotes $ responseBody $ unwrapJsonResponse response
queryBttvEmotes manager (Just (T.uncons -> Just ('#', channelName))) = do
  let encodeURI = escapeURIString (const False)
  request <-
    parseRequest $
    "https://api.betterttv.net/2/channels/" <> encodeURI (T.unpack channelName)
  response <- lift $ httpJson manager request
  hoistEither $ fmap bttvResEmotes $ responseBody $ unwrapJsonResponse response
queryBttvEmotes _ (Just invalidChannelName) =
  throwE $
  "Channel name " <> T.unpack invalidChannelName <> " does not start with #"

updateBttvEmotes ::
     Connection -> Manager -> Maybe T.Text -> ExceptT String IO ()
updateBttvEmotes dbConn manager channel = do
  lift $
    executeNamed
      dbConn
      [sql|DELETE FROM BttvEmotes WHERE channel IS :channel;|]
      [":channel" := channel]
  bttvEmotes <- queryBttvEmotes manager channel
  for_ bttvEmotes $ \emote ->
    lift $
    executeNamed
      dbConn
      [sql|INSERT INTO BttvEmotes (name, imageUrl, channel)
           VALUES (:name, :imageUrl, :channel)|]
      [ ":name" := bttvEmoteName emote
      , ":imageUrl" := bttvEmoteImageUrl emote
      , ":channel" := channel
      ]

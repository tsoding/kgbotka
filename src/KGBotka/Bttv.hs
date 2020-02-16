{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module KGBotka.Bttv
  ( updateBttvEmotes
  , getBttvEmoteByName
  , BttvEmote(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Irc.Identifier (idText)
import KGBotka.Http
import KGBotka.TwitchAPI
import Network.HTTP.Client
import Network.URI

data BttvEmote = BttvEmote
  { bttvEmoteName :: T.Text
  , bttvEmoteImageUrl :: T.Text
  , bttvEmoteChannel :: Maybe TwitchIrcChannel
  }

instance FromRow BttvEmote where
  fromRow = BttvEmote <$> field <*> field <*> field

updateBttvEmoteChannel :: Maybe TwitchIrcChannel -> BttvEmote -> BttvEmote
updateBttvEmoteChannel channel bttvEmote =
  bttvEmote {bttvEmoteChannel = channel}

newtype BttvRes = BttvRes
  { bttvResEmotes :: [BttvEmote]
  }

instance FromJSON BttvRes where
  parseJSON (Object v) = BttvRes <$> v .: "emotes"
  parseJSON invalid = typeMismatch "BttvRes" invalid

instance FromJSON BttvEmote where
  parseJSON (Object v) = BttvEmote <$> code <*> url <*> return Nothing
    where
      code = v .: "code"
      url =
        (\id' -> "https://cdn.betterttv.net/emote/" <> id' <> "/3x") <$>
        (v .: "id")
  parseJSON invalid = typeMismatch "BttvEmote" invalid

queryBttvEmotes ::
     Manager -> Maybe TwitchIrcChannel -> ExceptT String IO [BttvEmote]
queryBttvEmotes manager Nothing = do
  request <- parseRequest "https://api.betterttv.net/2/emotes"
  response <- lift $ httpJson manager request
  except $ fmap bttvResEmotes $ responseBody $ unwrapJsonResponse response
queryBttvEmotes manager channel'@(Just (TwitchIrcChannel (idText -> channel))) =
  case T.uncons channel of
    Just ('#', channelName) -> do
      let encodeURI = escapeURIString (const False)
      request <-
        parseRequest $
        "https://api.betterttv.net/2/channels/" <>
        encodeURI (T.unpack channelName)
      response <- lift $ httpJson manager request
      except $
        fmap (map (updateBttvEmoteChannel channel') . bttvResEmotes) $
        responseBody $ unwrapJsonResponse response
    _ ->
      let invalidChannelName = channel
       in throwE $
          "Channel name " <> T.unpack invalidChannelName <>
          " does not start with #"

updateBttvEmotes ::
     Connection -> Manager -> Maybe TwitchIrcChannel -> ExceptT String IO ()
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
      , ":channel" := bttvEmoteChannel emote
      ]

getBttvEmoteByName ::
     Connection -> T.Text -> Maybe TwitchIrcChannel -> MaybeT IO BttvEmote
getBttvEmoteByName dbConn name channel =
  MaybeT
    (listToMaybe <$>
     queryNamed
       dbConn
       [sql|SELECT name, imageUrl, channel FROM BttvEmotes
            WHERE (channel is :channel OR channel is NULL)
            AND name is :name|]
       [":channel" := channel, ":name" := name])

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module KGBotka.Ffz
  ( FfzEmote(..)
  , updateFfzEmotes
  , getFfzEmoteByName
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import KGBotka.TwitchAPI
import Network.HTTP.Client

data FfzEmote = FfzEmote
  { ffzEmoteName :: T.Text
  , ffzEmoteImageUrl :: T.Text
  , ffzEmoteChannel :: Maybe TwitchIrcChannel
  }

-- TODO(#243): updateFfzEmotes does not handle channels that do no exist on FFZ
updateFfzEmoteChannel :: Maybe TwitchIrcChannel -> FfzEmote -> FfzEmote
updateFfzEmoteChannel channel emote = emote {ffzEmoteChannel = channel}

instance FromRow FfzEmote where
  fromRow = FfzEmote <$> field <*> field <*> field

newtype FfzSet = FfzSet
  { ffzSetEmotes :: [FfzEmote]
  }

instance FromJSON FfzSet where
  parseJSON (Object v) = FfzSet <$> (v .: "emoticons")
  parseJSON invalid = typeMismatch "FfzSet" invalid

data FfzGlobalRes = FfzGlobalRes
  { ffzGlobalResDefaultSets :: [Int]
  , ffzGlobalResSets :: M.Map T.Text FfzSet
  }

instance FromJSON FfzGlobalRes where
  parseJSON (Object v) = FfzGlobalRes <$> v .: "default_sets" <*> v .: "sets"
  parseJSON invalid = typeMismatch "FfzGlobalRes" invalid

newtype FfzRes = FfzRes
  { ffzResEmotes :: [FfzEmote]
  }

instance FromJSON FfzEmote where
  parseJSON (Object v) =
    FfzEmote <$> v .: "name" <*> (v .: "urls" >>= maxUrl) <*> return Nothing
    where
      maxUrl :: Value -> Parser T.Text
      maxUrl (Object v') =
        (\case
           Nothing -> typeMismatch "List of FFZ emote urls" $ Object v'
           (Just x) -> ("https:" <>) <$> parseJSON x) =<<
        pure ((`HM.lookup` v') =<< idx)
        where
          idx = listToMaybe $ sortOn Down $ HM.keys v'
      maxUrl invalid = typeMismatch "FfzEmote" invalid
  parseJSON invalid = typeMismatch "FfzEmote" invalid

instance FromJSON FfzRes where
  parseJSON (Object v) =
    FfzRes <$> do
      setId <- v .: "room" >>= (.: "set") :: Parser Int
      v .: "sets" >>= (.: (T.pack $ show setId)) >>= (.: "emoticons")
  parseJSON invalid = typeMismatch "FfzRes" invalid

queryFfzEmotes ::
     Manager -> Maybe TwitchIrcChannel -> ExceptT String IO [FfzEmote]
queryFfzEmotes manager Nothing
  -- @uri
 = do
  request <- parseRequest "https://api.frankerfacez.com/v1/set/global"
  ffzRes <- ExceptT (eitherDecode . responseBody <$> httpLbs request manager)
  return $
    concatMap ffzSetEmotes $
    mapMaybe
      (\setId -> M.lookup (T.pack $ show setId) $ ffzGlobalResSets ffzRes) $
    ffzGlobalResDefaultSets ffzRes
queryFfzEmotes manager (Just channel) =
  case T.uncons $ twitchIrcChannelText channel of
    Just ('#', channelName)
      -- @uri
     -> do
      request <-
        parseRequest $
        "https://api.frankerfacez.com/v1/room/" <> T.unpack channelName
      response <- lift (eitherDecode . responseBody <$> httpLbs request manager)
      except $
        map (updateFfzEmoteChannel $ Just channel) . ffzResEmotes <$> response
    _ ->
      throwE $
      "Channel name " <> T.unpack (twitchIrcChannelText channel) <>
      " does not start with #"

updateFfzEmotes ::
     Connection -> Manager -> Maybe TwitchIrcChannel -> ExceptT String IO ()
updateFfzEmotes dbConn manager channel = do
  lift $
    executeNamed
      dbConn
      [sql|DELETE FROM FfzEmotes WHERE channel is :channel;|]
      [":channel" := channel]
  ffzEmotes <- queryFfzEmotes manager channel
  for_ ffzEmotes $ \emote ->
    lift $
    executeNamed
      dbConn
      [sql|INSERT INTO FfzEmotes (name, imageUrl, channel)
           VALUES (:name, :imageUrl, :channel)|]
      [ ":name" := ffzEmoteName emote
      , ":imageUrl" := ffzEmoteImageUrl emote
      , ":channel" := ffzEmoteChannel emote
      ]

getFfzEmoteByName ::
     Connection -> T.Text -> Maybe TwitchIrcChannel -> MaybeT IO FfzEmote
getFfzEmoteByName dbConn name channel =
  MaybeT
    (listToMaybe <$>
     queryNamed
       dbConn
       [sql|SELECT name, imageUrl, channel FROM FfzEmotes
            WHERE (channel is :channel OR channel is NULL)
            AND name is :name|]
       [":channel" := channel, ":name" := name])

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Asciify
  ( asciifyUrl
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Louis
import qualified Network.HTTP.Client as HTTP

fromCache :: Connection -> T.Text -> MaybeT IO T.Text
fromCache dbConn url =
  MaybeT
    (fmap fromOnly . listToMaybe <$>
     queryNamed
       dbConn
       [sql|SELECT image FROM AsciifyUrlCache WHERE url = :url|]
       [":url" := url])

fromUrl :: HTTP.Manager -> T.Text -> ExceptT String IO T.Text
fromUrl manager url = do
  request <- HTTP.parseRequest $ T.unpack url
  response <- lift $ HTTP.httpLbs request manager
  fmap T.unwords $
    except $ braillizeByteString $ BS.toStrict $ HTTP.responseBody response

cacheImage :: Connection -> T.Text -> T.Text -> IO ()
cacheImage dbConn url image =
  executeNamed
    dbConn
    [sql|INSERT INTO AsciifyUrlCache (url, image) VALUES (:url, :image)|]
    [":url" := url, ":image" := image]

asciifyUrl :: Connection -> HTTP.Manager -> T.Text -> ExceptT String IO T.Text
asciifyUrl dbConn manager url
  -- NOTE: `Nothing` from `fromCache` indicates the cache miss. We set
  -- the error to empty string because `ExceptT` expects exceptions to
  -- be `Monoid`s and simply `mappend`s them together. So by setting
  -- error of `fromCache` to empty string we don't disturb the error
  -- of `fromUrl` and `cacheImage`
 =
  maybeToExceptT "" (fromCache dbConn url) <|> do
    image <- fromUrl manager url
    lift $ cacheImage dbConn url image
    return image

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Asciify
  ( asciifyUrl
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Extra
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Louis
import qualified Network.HTTP.Client as HTTP

fromCache :: Connection -> T.Text -> ExceptT String IO T.Text
fromCache dbConn url
  -- TODO: weird error concatination behaviour
 =
  maybeToExceptT "Asciify URL cache miss" $
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
    hoistEither $ braillizeByteString $ BS.toStrict $ HTTP.responseBody response

cacheImage :: Connection -> T.Text -> T.Text -> IO ()
cacheImage dbConn url image =
  executeNamed
    dbConn
    [sql|INSERT INTO AsciifyUrlCache (url, image) VALUES (:url, :image)|]
    [":url" := url, ":image" := image]

asciifyUrl :: Connection -> HTTP.Manager -> T.Text -> ExceptT String IO T.Text
asciifyUrl dbConn manager url =
  fromCache dbConn url <|> do
    image <- fromUrl manager url
    lift $ cacheImage dbConn url image
    return image

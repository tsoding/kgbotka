{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module KGBotka.Xkcd where

import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple (NamedParam(..))
import Database.SQLite.Simple.QQ
import qualified Network.HTTP.Client as HTTP
import Text.Printf

type XkcdNum = Int64

data Xkcd = Xkcd
  { xkcdNum :: XkcdNum
  , xkcdTitle :: T.Text
  , xkcdImg :: T.Text
  , xkcdAlt :: T.Text
  , xkcdTranscript :: T.Text
  } deriving (Eq, Show)

instance FromJSON Xkcd where
  parseJSON (Object v) =
    Xkcd <$> v .: "num" <*> v .: "title" <*> v .: "img" <*> v .: "alt" <*>
    v .: "transcript"
  parseJSON invalid = typeMismatch "Xkcd" invalid

queryXkcdByURL :: HTTP.Manager -> String -> IO Xkcd
queryXkcdByURL manager url = do
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request manager
  case eitherDecode $ HTTP.responseBody response of
    Right xkcd -> return xkcd
    Left errorMessage -> error errorMessage

queryCurrentXkcd :: HTTP.Manager -> IO Xkcd
queryCurrentXkcd manager = queryXkcdByURL manager "https://xkcd.com/info.0.json"

queryXkcdById :: HTTP.Manager -> XkcdNum -> IO Xkcd
queryXkcdById manager num =
  queryXkcdByURL manager $ printf "https://xkcd.com/%d/info.0.json" num

dumpXkcdToDb :: Xkcd -> Sqlite.Connection -> IO ()
dumpXkcdToDb Xkcd { xkcdNum = num
                  , xkcdTitle = title
                  , xkcdImg = img
                  , xkcdAlt = alt
                  , xkcdTranscript = transcript
                  } dbConn =
  Sqlite.executeNamed
    dbConn
    [sql|INSERT INTO xkcd (num, title, img, alt, transcript)
         VALUES (:num, :title, :img, :alt, :transcript)|]
    [ ":num" := num
    , ":title" := title
    , ":img" := img
    , ":alt" := alt
    , ":transcript" := transcript
    ]

instance Sqlite.FromRow Xkcd where
  fromRow =
    Xkcd <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*>
    Sqlite.field

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

getLastDumpedXkcd :: Sqlite.Connection -> IO (Maybe Xkcd)
getLastDumpedXkcd dbConn =
  listToMaybe <$>
  Sqlite.queryNamed
    dbConn
    [sql|select num, title, img, alt, transcript
         from xkcd order by num desc limit 1|]
    []

stopChars :: String
stopChars = "[]{}:.?!'"

textAsTerms :: T.Text -> [T.Text]
textAsTerms = map T.toUpper . T.words . T.filter (`notElem` stopChars)

indexXkcd :: Sqlite.Connection -> Xkcd -> IO ()
indexXkcd dbConn xkcd = do
  let terms =
        textAsTerms (xkcdTranscript xkcd) <> textAsTerms (xkcdTitle xkcd) <>
        textAsTerms (xkcdAlt xkcd)
  traverse_
    (\g ->
       let term = head g
           freq = length g
        in Sqlite.executeNamed
             dbConn
             [sql|INSERT INTO xkcd_tf_idf (term, freq, num)
                VALUES (:term, :freq, :num);|]
             [":term" := term, ":freq" := freq, ":num" := xkcdNum xkcd]) $
    group $ sort terms

-- TODO: there is no way to update xkcd_tf_idf from within the bot
searchXkcdInDbByTerm :: Sqlite.Connection -> T.Text -> IO (Maybe Xkcd)
searchXkcdInDbByTerm dbConn term =
  listToMaybe <$>
  Sqlite.queryNamed
    dbConn
    [sql|SELECT xkcd.num,
                xkcd.title,
                xkcd.img,
                xkcd.alt,
                xkcd.transcript
         FROM xkcd_tf_idf
         INNER JOIN xkcd ON xkcd_tf_idf.num = xkcd.num
         WHERE xkcd_tf_idf.term = upper(:term)
         ORDER BY xkcd_tf_idf.freq DESC;|]
    [":term" := term]

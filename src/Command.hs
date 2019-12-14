module Command
  ( CommandCall(..)
  , parseCommandCall
  , Command(..)
  , commandByName
  ) where

import qualified Data.Text as T
import Data.Char
import qualified Database.SQLite.Simple as Sqlite
import Data.Maybe

data Command =
  Command Int
          T.Text
  deriving (Show)

instance Sqlite.FromRow Command where
  fromRow = Command <$> Sqlite.field <*> Sqlite.field

commandByName :: Sqlite.Connection -> T.Text -> IO (Maybe Command)
commandByName conn name =
  listToMaybe <$>
  Sqlite.queryNamed conn queryText [":commandName" Sqlite.:= name]
  where
    queryText =
      "SELECT c.id, c.code \
      \FROM Command c \
      \INNER JOIN CommandName cn ON c.id = cn.commandId \
      \WHERE cn.name = :commandName;"

data CommandCall = CommandCall
  { ccName :: T.Text
  , ccArgs :: T.Text
  } deriving (Eq, Show)

parseCommandCall :: T.Text -> T.Text -> Maybe CommandCall
parseCommandCall prefix text =
  uncurry CommandCall . fmap T.strip . T.span isAlphaNum <$>
  T.stripPrefix prefix (T.dropWhile isSpace text)

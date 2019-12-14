module Command
  ( Command(..)
  , parseCommand
  ) where

import qualified Data.Text as T
import Data.Char

data Command = Command
  { commandName :: T.Text
  , commandArgs :: T.Text
  } deriving (Eq, Show)

parseCommand :: T.Text -> T.Text -> Maybe Command
parseCommand prefix text =
  uncurry Command . fmap T.strip . T.span isAlphaNum <$>
  T.stripPrefix prefix (T.dropWhile isSpace text)

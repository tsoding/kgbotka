{-# LANGUAGE OverloadedStrings #-}

module Command
  ( CommandCall(..)
  , parseCommandCall
  , Command(..)
  , commandByName
  , addCommand
  , addCommandName
  , deleteCommandByName
  , deleteCommandName
  ) where

import qualified Data.Text as T
import Data.Char
import Database.SQLite.Simple
import Data.Maybe

data Command =
  Command Int
          T.Text
  deriving (Show)


instance FromRow Command where
  fromRow = Command <$> field <*> field

commandByName :: Connection -> T.Text -> IO (Maybe Command)
commandByName conn name =
  listToMaybe <$>
  queryNamed conn queryText [":commandName" := name]
  where
    queryText =
      "SELECT c.id, c.code \
      \FROM Command c \
      \INNER JOIN CommandName cn ON c.id = cn.commandId \
      \WHERE cn.name = :commandName;"

addCommand :: Connection -> T.Text -> T.Text -> IO ()
addCommand dbConn name code = do
  executeNamed
    dbConn
    "INSERT INTO Command (code) VALUES (:commandCode)"
    [":commandCode" := code]
  commandId <- lastInsertRowId dbConn
  executeNamed
    dbConn
    "INSERT INTO CommandName (name, commandId)\
    \VALUES (:commandName, :commandId)"
    [":commandName" := name, ":commandId" := commandId]

deleteCommandByName :: Connection -> T.Text -> IO ()
deleteCommandByName = undefined -- TODO: deleteCommandByName not implemented

deleteCommandName :: Connection -> T.Text -> IO ()
deleteCommandName = undefined -- TODO: deleteCommandName not implemented

addCommandName :: Connection -> T.Text -> T.Text -> IO ()
addCommandName dbConn alias name  = do
  command <- commandByName dbConn name
  case command of
    Just (Command commandId _) -> 
      executeNamed 
        dbConn 
        "INSERT INTO CommandName (name, commandId)\
        \VALUES (:commandName, :commandId)"
        [":commandName" := alias, ":commandId" := commandId]
    Nothing -> return ()

data CommandCall = CommandCall
  { ccName :: T.Text
  , ccArgs :: T.Text
  } deriving (Eq, Show)

parseCommandCall :: T.Text -> T.Text -> Maybe CommandCall
parseCommandCall prefix text =
  uncurry CommandCall . fmap T.strip . T.span isAlphaNum <$>
  T.stripPrefix prefix (T.dropWhile isSpace text)

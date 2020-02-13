{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module KGBotka.Command
  ( CommandCall(..)
  , parseCommandCall
  , parseCommandPipe
  , Command(..)
  , commandByName
  , addCommand
  , addCommandName
  , deleteCommandByName
  , deleteCommandName
  , ccArgsModify
  , logCommand
  , isCommandCooleddown
  ) where

import Data.Char
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import KGBotka.TwitchAPI
import Data.Time
import Data.Fixed

data Command = Command
  { commandId :: Int
  , commandCode :: T.Text
  , commandUserCooldown :: Int
  } deriving (Show)

data CommandLog = CommandLog
  { commandLogUserTwitchId :: TwitchUserId
  , commandLogCommandId :: Int
  , commandLogCommandArgs :: T.Text
  , commnadLogTimestamp :: UTCTime
  } deriving (Show)

logCommand :: Connection -> TwitchUserId -> Int -> T.Text -> IO ()
logCommand dbConn userTwitchId commandIdent commandArgs =
  executeNamed
    dbConn
    [sql|INSERT INTO CommandLog (userTwitchId, commandId, commandArgs)
         VALUES (:userTwitchId, :commandId, :commandArgs)|]
    [ ":userTwitchId" := userTwitchId
    , ":commandId" := commandIdent
    , ":commandArgs" := commandArgs
    ]

instance FromRow Command where
  fromRow = Command <$> field <*> field <*> field

commandByName :: Connection -> T.Text -> IO (Maybe Command)
commandByName conn name =
  listToMaybe <$> queryNamed conn queryText [":commandName" := name]
  where
    queryText =
      [sql|SELECT c.id, c.code, c.user_cooldown_ms
           FROM Command c
           INNER JOIN CommandName cn ON c.id = cn.commandId
           WHERE cn.name = :commandName;|]

addCommand :: Connection -> T.Text -> T.Text -> IO ()
addCommand dbConn name code = do
  executeNamed
    dbConn
    "INSERT INTO Command (code) VALUES (:commandCode)"
    [":commandCode" := code]
  ident <- lastInsertRowId dbConn
  executeNamed
    dbConn
    "INSERT INTO CommandName (name, commandId) \
    \VALUES (:commandName, :commandId)"
    [":commandName" := name, ":commandId" := ident]

deleteCommandById :: Connection -> Int -> IO ()
deleteCommandById dbConn ident =
  executeNamed
    dbConn
    "DELETE FROM Command WHERE id = :commandId"
    [":commandId" := ident]

deleteCommandByName :: Connection -> T.Text -> IO ()
deleteCommandByName dbConn name =
  commandByName dbConn name >>=
  maybe (return ()) (deleteCommandById dbConn . commandId)

deleteCommandName :: Connection -> T.Text -> IO ()
deleteCommandName dbConn name =
  executeNamed
    dbConn
    "DELETE FROM CommandName WHERE name = :commandName"
    [":commandName" := name]

addCommandName :: Connection -> T.Text -> T.Text -> IO ()
addCommandName dbConn alias name = do
  command <- commandByName dbConn name
  case command of
    Just (Command {commandId = ident}) ->
      executeNamed
        dbConn
        "INSERT INTO CommandName (name, commandId)\
        \VALUES (:commandName, :commandId)"
        [":commandName" := alias, ":commandId" := ident]
    Nothing -> return ()

data CommandCall = CommandCall
  { ccName :: T.Text
  , ccArgs :: T.Text
  } deriving (Eq, Show)

parseCommandPipe :: T.Text -> T.Text -> T.Text -> [CommandCall]
parseCommandPipe callPrefix pipeSuffix source =
  fromMaybe [] $
  mapM (parseCommandCall callPrefix) $ T.splitOn pipeSuffix source

parseCommandCall :: T.Text -> T.Text -> Maybe CommandCall
parseCommandCall prefix source =
  uncurry CommandCall . fmap T.strip . T.span isAlphaNum <$>
  T.stripPrefix prefix (T.dropWhile isSpace source)

ccArgsModify :: (T.Text -> T.Text) -> CommandCall -> CommandCall
ccArgsModify f cc = cc {ccArgs = f $ ccArgs cc}

isCommandCooleddown :: Connection -> TwitchUserId -> Int -> IO Bool
isCommandCooleddown dbConn userTwitchId commandIdent = do
  x <-
    listToMaybe <$>
    queryNamed
      dbConn
      [sql|SELECT cl.timestamp, c.user_cooldown_ms
           FROM CommandLog cl
           JOIN Command c ON c.id = cl.commandId
           WHERE cl.userTwitchId = :userTwitchId
             AND cl.commandId = :commandIdent
           ORDER BY cl.timestamp DESC
           LIMIT 1; |]
      [":userTwitchId" := userTwitchId, ":commandIdent" := commandIdent]
  case x of
    Just (timestamp, cooldown) -> do
      now <- getCurrentTime
      return
        ((nominalDiffTimeToSeconds $ diffUTCTime now timestamp) >
         MkFixed (cooldown * 1000 * 1000 * 1000 * 1000))
    Nothing -> return True

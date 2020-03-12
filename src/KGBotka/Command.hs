{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

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
  , CallPrefix(..)
  , PipeSuffix(..)
  ) where

import Data.Char
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import KGBotka.TwitchAPI
import KGBotka.Sqlite

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

addCommand :: ProvidesDatabase pd => pd -> T.Text -> T.Text -> IO ()
addCommand (getSqliteConnection -> dbConn) name code = do
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

deleteCommandByName :: ProvidesDatabase s => s -> T.Text -> IO ()
deleteCommandByName (getSqliteConnection -> dbConn) name =
  commandByName dbConn name >>=
  maybe (return ()) (deleteCommandById dbConn . commandId)

deleteCommandName :: ProvidesDatabase s => s -> T.Text -> IO ()
deleteCommandName (getSqliteConnection -> dbConn) name =
  executeNamed
    dbConn
    "DELETE FROM CommandName WHERE name = :commandName"
    [":commandName" := name]

addCommandName :: ProvidesDatabase pd => pd -> T.Text -> T.Text -> IO ()
addCommandName (getSqliteConnection -> dbConn) alias name = do
  command <- commandByName dbConn name
  case command of
    Just Command {commandId = ident} ->
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

newtype CallPrefix =
  CallPrefix T.Text

newtype PipeSuffix =
  PipeSuffix T.Text

parseCommandPipe :: CallPrefix -> PipeSuffix -> T.Text -> [CommandCall]
parseCommandPipe callPrefix (PipeSuffix pipeSuffix) source =
  fromMaybe [] $
  mapM (parseCommandCall callPrefix) $ T.splitOn pipeSuffix source

parseCommandCall :: CallPrefix -> T.Text -> Maybe CommandCall
parseCommandCall (CallPrefix prefix) source =
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
    Just (timestamp, cooldownMs) -> do
      now <- getCurrentTime
      let diffSec = realToFrac (diffUTCTime now timestamp) :: Double
      let cooldownSec = fromIntegral (cooldownMs :: Integer) / 1000.0
      return $ diffSec > cooldownSec
    Nothing -> return True

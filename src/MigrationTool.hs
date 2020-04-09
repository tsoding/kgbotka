{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import KGBotka.Command
import KGBotka.Migration
import System.Environment

-- TODO(#141): MigrationTool does not convert Friday queue
-- TODO: MigrationTool does not convert Twitch and Discord logs
-- TODO: MigrationTool does not convert Trusted users
-- TODO: MigrationTool does not convert quote database
-- TODO: populateHyperNerdBuiltinCommands does not support !trust and !untrust commands
-- TODO: populateHyperNerdBuiltinCommands does not support !updatebttv !updateffz
populateHyperNerdBuiltinCommands :: Connection -> IO ()
populateHyperNerdBuiltinCommands dbConn = do
  addCommand dbConn "addalias" "%addalias(%1)"
  addCommand dbConn "addcmd" "%addcmd(%1)"
  addCommand dbConn "asciify" "%asciify(%1)"
  addCommand dbConn "calc" "%calc(%1)"
  addCommand dbConn "cycle" "%cycle(%1)"
  addCommand dbConn "derussify" "%derussify(%1)"
  addCommand dbConn "friday" "%friday(%1)"
  addCommand dbConn "help" "%help(%1)"
  addCommand dbConn "markov" "%markov(%1)"
  addCommand dbConn "nextstream" "%nextstream(%1)"
  addCommand dbConn "nextvideo" "%nextvideo(%1)"
  addCommand dbConn "omega" "%omega(%1)"
  addCommand dbConn "russify" "%russify(%1)"
  addCommand dbConn "showcmd" "%showcmd(%1)"
  addCommand dbConn "updcmd" "%updcmd(%1)"
  addCommand dbConn "vanish" "%vanish(%1)"
  addCommand dbConn "version" "%version(%1)"
  addCommand dbConn "video" "%video(%1)"
  addCommand dbConn "videocount" "%videocount(%1)"
  addCommand dbConn "videoq" "%videoq(%1)"

-- TODO: MigrationTool should rather called ConvertionTool or something like that.
-- TODO: Perform database conversion on CI for testing purposes
-- TODO: convertCommands does not convert the amount of times the command was executed
convertCommands :: Connection -> IO ()
convertCommands dbConn = do
  legacyCommands <-
    queryNamed
      dbConn
      [sql|select ep1.propertyText, ep2.propertyText
           from EntityProperty ep1
           inner join EntityProperty ep2
           on ep1.entityId = ep2.entityId
           where ep1.entityName = 'CustomCommand'
           and ep1.propertyName = 'name'
           and ep2.propertyName = 'message'; |]
      []
  traverse_ (uncurry $ addCommand dbConn) legacyCommands

convertAliases :: Connection -> IO ()
convertAliases dbConn = do
  legacyAliases <-
    queryNamed
      dbConn
      [sql|select ep1.propertyText, ep2.propertyText
           from EntityProperty ep1
           inner join EntityProperty ep2
           on ep1.entityId = ep2.entityId
           where ep1.entityName = 'Alias'
           and ep1.propertyName = 'name'
           and ep2.propertyName = 'redirect'|]
      []
  -- TODO: convertAliases silently ignores non existing command
  --   It should print a warning or something
  traverse_ (uncurry $ addCommandName dbConn) legacyAliases

main :: IO ()
main = do
  args <- getArgs
  -- TODO: make a backup of the database before trying to convert it
  case args of
    dbPath:_ -> do
      withConnection dbPath $ \dbConn ->
        withTransaction dbConn $ do
          putStrLn "[INFO] Preparing the migration table..."
          executeNamed dbConn [sql|DROP TABLE Migrations|] []
          migrateDatabase dbConn kgbotkaMigrations
          executeNamed dbConn [sql|DROP TABLE EntityId|] []
          putStrLn "[INFO] Populating HyperNerd builtin commands..."
          populateHyperNerdBuiltinCommands dbConn
          putStrLn "[INFO] Converting commands..."
          convertCommands dbConn
          putStrLn "[INFO] Converting aliases..."
          convertAliases dbConn
      putStrLn "OK"
    _ -> error "Usage: MigrationTool <dbPath>"

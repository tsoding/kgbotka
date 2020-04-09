{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.Environment
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import KGBotka.Migration
import KGBotka.Command
import Data.Foldable

-- TODO: MigrationTool should rather called ConvertionTool or something like that.

-- TODO: Perform database conversion on CI

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
          putStrLn "[INFO] Converting commands..."
          convertCommands dbConn
      putStrLn "OK"
    _ -> error "Usage: MigrationTool <dbPath>"

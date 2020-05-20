{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable
import Data.Functor
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import KGBotka.Command
import KGBotka.Migration
import System.Directory
import System.Environment

-- TODO(#145): MigrationTool does not convert Trusted users
-- TODO(#146): MigrationTool does not convert quote database
-- TODO(#147): populateHyperNerdBuiltinCommands does not support !trust and !untrust commands
-- TODO(#148): populateHyperNerdBuiltinCommands does not support !updatebttv !updateffz
populateHyperNerdBuiltinCommands :: Connection -> IO ()
populateHyperNerdBuiltinCommands dbConn = do
  void $ addCommand dbConn "addalias" "%addalias(%1)"
  void $ addCommand dbConn "addcmd" "%addcmd(%1)"
  void $ addCommand dbConn "asciify" "%asciify(%1)"
  void $ addCommand dbConn "calc" "%calc(%1)"
  void $ addCommand dbConn "cycle" "%cycle(%1)"
  void $ addCommand dbConn "derussify" "%derussify(%1)"
  void $ addCommand dbConn "friday" "%friday(%1)"
  void $ addCommand dbConn "help" "%help(%1)"
  void $ addCommand dbConn "markov" "%markov(%1)"
  void $ addCommand dbConn "nextstream" "%nextstream(%1)"
  void $ addCommand dbConn "nextvideo" "%nextvideo(%1)"
  void $ addCommand dbConn "omega" "%omega(%1)"
  void $ addCommand dbConn "russify" "%russify(%1)"
  void $ addCommand dbConn "showcmd" "%showcmd(%1)"
  void $ addCommand dbConn "updcmd" "%updcmd(%1)"
  void $ addCommand dbConn "vanish" "%vanish(%1)"
  void $ addCommand dbConn "version" "%version(%1)"
  void $ addCommand dbConn "video" "%video(%1)"
  void $ addCommand dbConn "videocount" "%videocount(%1)"
  void $ addCommand dbConn "videoq" "%videoq(%1)"

-- TODO(#199): go through all of the conversion queries and check the baseline amount of entities
-- TODO(#149): MigrationTool should rather called ConvertionTool or something like that.
-- TODO(#150): Perform database conversion on CI for testing purposes
convertCommands :: Connection -> IO ()
convertCommands dbConn = do
  legacyCommands <-
    queryNamed
      dbConn
      [sql|select name.propertyText,
                  message.propertyText,
                  times.propertyInt
           from (select entityId, entityName from EntityProperty
                 where entityName = 'CustomCommand'
                 group by entityId) commands
           left join EntityProperty name
                  on (commands.entityId = name.entityId and
                      commands.entityName = name.entityName and
                      name.propertyName = 'name')
           left join EntityProperty message
                  on (commands.entityId = message.entityId and
                      commands.entityName = message.entityName and
                      message.propertyName = 'message')
           left join EntityProperty times
                  on (commands.entityId = times.entityId and
                      commands.entityName = times.entityName and
                      times.propertyName = 'times');|]
      []
  traverse_
    (\(name, code, times) -> do
       commandIdent <- addCommand dbConn name code
       executeNamed
         dbConn
         [sql|update Command
              set times = :times
              where id = :commandId|]
         [":times" := (times :: Int), ":commandId" := commandIdent])
    legacyCommands

convertAliases :: Connection -> IO ()
convertAliases dbConn = do
  legacyAliases <-
    queryNamed
      dbConn
      [sql|select name.propertyText, redirect.propertyText
           from (select entityId, entityName from EntityProperty
                 where entityName = 'Alias'
                 group by entityId) alias
           left join EntityProperty name
                  on (alias.entityId = name.entityId and
                      alias.entityName = name.entityName and
                      name.propertyName = 'name')
           left join EntityProperty redirect
                  on (alias.entityId = redirect.entityId and
                      alias.entityName = redirect.entityName and
                      redirect.propertyName = 'redirect');|]
      []
  -- TODO(#152): convertAliases silently ignores non existing command
  --   It should print a warning or something
  traverse_ (uncurry $ addCommandName dbConn) legacyAliases

-- TODO(#195): document limitations of convertTwitchLogs
--   Roles are not converted (not available)
--   Messages without timestamps are ignored
convertTwitchLogs :: Connection -> IO ()
convertTwitchLogs dbConn =
  executeNamed
    dbConn
    [sql|insert into TwitchLog (channel,
                                senderTwitchName,
                                senderTwitchRoles ,
                                senderTwitchBadgeRoles,
                                message,
                                messageTime)
         select substr(channel.propertyText,
                       16,
                       length(channel.propertyText) - 16),
                user.propertyText,
                '[]',
                '[]',
                msg.propertyText,
                timestamp.propertyUTCTime
         from (select * from EntityProperty
               where entityName = 'LogRecord'
               group by entityId) record
         left join EntityProperty user
                on (record.entityId = user.entityId and
                    record.entityName = user.entityName and
                    user.propertyName = 'user')
         left join EntityProperty timestamp
                on (record.entityId = timestamp.entityId and
                    record.entityName = timestamp.entityName and
                    timestamp.propertyName = 'timestamp')
         left join EntityProperty msg
                on (record.entityId = msg.entityId and
                    record.entityName = msg.entityName and
                    msg.propertyName = 'msg')
         left join EntityProperty channel
                on (record.entityId = channel.entityId and
                    record.entityName = channel.entityName and
                    channel.propertyName = 'channel')
         where channel.propertyText like 'TwitchChannel "%"'
               -- NOTE: At some very brief moment of time HyperNerd
               -- was not saving timestamps. That was very long time
               -- ago and very brief, so if such messages occur, we can
               -- simply neglect them
           and timestamp.propertyUTCTime is not NULL;|]
    []

-- TODO(#196): document limitations of convertDiscordLogs
--   - guildId is not converted (not available)
--   - senderDiscordId is not converted (not available)
-- TODO(#203): convertDiscordLogs uses old conversion style
convertDiscordLogs :: Connection -> IO ()
convertDiscordLogs dbConn =
  executeNamed
    dbConn
    [sql|insert into DiscordLog (guildId,
                                 channelId,
                                 senderDiscordId,
                                 senderDiscordDisplayName,
                                 message,
                                 messageTime)
         select NULL,
                substr(channel.propertyText, 16),
                NULL,
                user.propertyText,
                msg.propertyText,
                timestamp.propertyUTCTime
         from (select * from EntityProperty
               where entityName = 'LogRecord'
               group by entityId) record
         left join EntityProperty user
                on (record.entityId = user.entityId and
                    record.entityName = user.entityName and
                    user.propertyName = 'user')
         left join EntityProperty timestamp
                on (record.entityId = timestamp.entityId and
                    record.entityName = timestamp.entityName and
                    timestamp.propertyName = 'timestamp')
         left join EntityProperty msg
                on (record.entityId = msg.entityId and
                    record.entityName = msg.entityName and
                    msg.propertyName = 'msg')
         left join EntityProperty channel
                on (record.entityId = channel.entityId and
                    record.entityName = channel.entityName and
                    channel.propertyName = 'channel')
         where channel.propertyText like 'DiscordChannel %'; |]
    []

-- TODO(#200): document limitations of convertFridayVideos
--   Special authorIds
convertFridayVideos :: Connection -> IO ()
convertFridayVideos dbConn =
  executeNamed
    dbConn
    [sql|insert into FridayVideo (submissionText,
                                  submissionTime,
                                  authorId,
                                  authorDisplayName,
                                  watchedAt)
         select subText.propertyText,
                subDate.propertyUTCTime,
                'Converted ' || author.propertyText,
                author.propertyText,
                watchedAt.propertyUTCTime
         from (select entityId, entityName
               from EntityProperty videos
               where entityName = 'FridayVideo'
               group by entityId) videos
         left join EntityProperty author
              on (author.entityId = videos.entityId and
                  author.entityName = videos.entityName and
                  author.propertyName = 'author')
         left join EntityProperty subDate
              on (subDate.entityId = videos.entityId and
                  subDate.entityName = videos.entityName and
                  subDate.propertyName = 'date')
         left join EntityProperty subText
              on (subText.entityId = videos.entityId and
                  subText.entityName = videos.entityName and
                  subText.propertyName = 'name')
         left join EntityProperty watchedAt
              on (watchedAt.entityId = videos.entityId and
                  watchedAt.entityName = videos.entityName and
                  watchedAt.propertyName = 'watchedAt'); |]
    []

main :: IO ()
main = do
  args <- getArgs
  case args of
    dbPath:_ -> do
      copyFile dbPath (dbPath ++ ".old")
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
          putStrLn "[INFO] Converting Twitch logs..."
          convertTwitchLogs dbConn
          putStrLn "[INFO] Converting Discord logs..."
          convertDiscordLogs dbConn
          putStrLn "[INFO] Converting Friday videos..."
          convertFridayVideos dbConn
      putStrLn "OK"
    _ -> error "Usage: MigrationTool <dbPath>"

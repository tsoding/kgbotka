{-# LANGUAGE OverloadedStrings #-}
module KGBotka.Repl where

import Control.Concurrent.STM
import qualified Data.Set as S
import Irc.Identifier (Identifier, idText, mkId)
import qualified Database.SQLite.Simple as Sqlite
import qualified Data.Text as T
import KGBotka.Queue
import qualified Network.HTTP.Client as HTTP
import KGBotka.Config
import Data.Maybe
import System.IO
import Control.Monad
import Data.Foldable
import KGBotka.Command
import KGBotka.Roles
import KGBotka.TwitchAPI

data ReplState = ReplState
  { replStateChannels :: TVar (S.Set Identifier)
  , replStateSqliteConnection :: Sqlite.Connection
  , replStateCurrentChannel :: Maybe T.Text
  , replStateCommandQueue :: WriteQueue ReplCommand
  , replStateConfigTwitch :: ConfigTwitch
  , replStateManager :: HTTP.Manager
  }

data ReplCommand
  = Say T.Text
        T.Text
  | JoinChannel T.Text
  | PartChannel Identifier

replThread :: ReplState -> IO ()
replThread state = do
  putStr $
    "[" <> T.unpack (fromMaybe "#" $ replStateCurrentChannel state) <> "]> "
  hFlush stdout
  cmd <- T.words . T.pack <$> getLine
  case (cmd, replStateCurrentChannel state) of
    ("cd":channel:_, _) ->
      replThread $ state {replStateCurrentChannel = Just channel}
    ("cd":_, _) -> replThread $ state {replStateCurrentChannel = Nothing}
    ("say":args, Just channel) -> do
      atomically $
        writeQueue (replStateCommandQueue state) $ Say channel $ T.unwords args
      replThread state
    ("say":_, Nothing) -> do
      putStrLn "No current channel to say anything to is selected"
      replThread state
    ("quit":_, _) -> return ()
    ("join":channel:_, _) -> do
      atomically $
        writeQueue (replStateCommandQueue state) $ JoinChannel channel
      replThread $ state {replStateCurrentChannel = Just channel}
    ("part":_, Just channel) -> do
      atomically $ do
        let channelId = mkId channel
        isMember <- S.member channelId <$> readTVar (replStateChannels state)
        when isMember $
          writeQueue (replStateCommandQueue state) $ PartChannel channelId
      replThread state
    ("ls":_, _) -> do
      traverse_ (putStrLn . T.unpack . idText) =<<
        S.toList <$> readTVarIO (replStateChannels state)
      replThread state
    ("addcmd":name:args, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ addCommand dbConn name (T.unwords args)
      replThread state
    ("addalias":alias:name:_, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ addCommandName dbConn alias name
      replThread state
    ("delcmd":name:_, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ deleteCommandByName dbConn name
      replThread state
    ("delalias":name:_, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ deleteCommandName dbConn name
      replThread state
    ("assrole":roleName:users, _) -> do
      let dbConn = replStateSqliteConnection state
      Sqlite.withTransaction dbConn $ do
        maybeRole <- getTwitchRoleByName dbConn roleName
        response <-
          HTTP.responseBody . unwrapJsonResponse <$>
          getUsersByLogins
            (replStateManager state)
            (configTwitchClientId $ replStateConfigTwitch state)
            users
        case (response, maybeRole) of
          (Right twitchUsers, Just role') ->
            traverse_
              (assTwitchRoleToUser dbConn (twitchRoleId role') .
               TwitchUserId . userId)
              twitchUsers
          (Left message, _) -> putStrLn $ "[ERROR] " <> message
          (_, Nothing) -> putStrLn "[ERROR] Such role does not exist"
      replThread state
    (unknown:_, _) -> do
      putStrLn $ T.unpack $ "Unknown command: " <> unknown
      replThread state
    _ -> replThread state

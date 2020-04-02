{-# LANGUAGE OverloadedStrings #-}

module KGBotka.GithubThread
  ( githubThread
  , GithubThreadParams(..)
  ) where

import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Database.SQLite.Simple as Sqlite
import KGBotka.Config
import KGBotka.Friday
import KGBotka.Log
import KGBotka.Queue
import KGBotka.Settings
import Network.HTTP.Client
import Text.Printf

data GithubThreadParams = GithubThreadParams
  { gtpSqliteConnection :: !(MVar Sqlite.Connection)
  , gtpManager :: !Manager
  , gtpLogQueue :: !(WriteQueue LogEntry)
  , gtpConfig :: !(Maybe ConfigGithub)
  }

instance ProvidesLogging GithubThreadParams where
  logQueue = gtpLogQueue

data GithubThreadState = GithubThreadState
  { gtsSqliteConnection :: !(MVar Sqlite.Connection)
  , gtsManager :: !Manager
  , gtsLogQueue :: !(WriteQueue LogEntry)
  , gtsConfig :: ConfigGithub
  }

instance ProvidesLogging GithubThreadState where
  logQueue = gtsLogQueue

githubThread :: GithubThreadParams -> IO ()
githubThread gtp@GithubThreadParams {gtpConfig = Just config} =
  githubThreadLoop
    GithubThreadState
      { gtsSqliteConnection = gtpSqliteConnection gtp
      , gtsManager = gtpManager gtp
      , gtsLogQueue = gtpLogQueue gtp
      , gtsConfig = config
      }
githubThread gtp =
  logEntry gtp $
  LogEntry "GITHUB" "[ERROR] GitHub configuration is not provided"

githubThreadLoop :: GithubThreadState -> IO ()
githubThreadLoop gts = do
  threadDelay $ 60 * 1000 * 1000
  fridayGist <-
    withMVar (gtsSqliteConnection gts) $ \conn ->
      Sqlite.withTransaction conn $
      runMaybeT $ do
        gistId <- MaybeT $ settingsFridayGithubGistId <$> fetchSettings conn
        gistText <- lift $ renderAllQueues <$> fetchAllQueues conn
        return (gistId, gistText)
  case fridayGist of
    Just (gistId, gistText) -> do
      logEntry gts $ LogEntry "GITHUB" "Updating Friday Video Queue gist..."
      -- TODO(#132): github thread update friday gist every minute regardless of whether it's even needed
      updateGistFile (gtsManager gts) (configGithubToken $ gtsConfig gts) $
        GistFile
          { gistFileId = gistId
          , gistFileText = gistText
          , gistFileName = "Friday.org"
          }
    Nothing ->
      logEntry gts $
      LogEntry
        "GITHUB"
        "[WARN] Tried to update Friday Video Queue, \
        \but the gist id is not setup"
  githubThreadLoop gts

data GistFile = GistFile
  { gistFileId :: T.Text
  , gistFileName :: T.Text
  , gistFileText :: T.Text
  }

updateGistFile :: Manager -> GithubToken -> GistFile -> IO ()
updateGistFile manager (GithubToken token) gistFile = do
  let payload =
        object
          [ "files" .=
            object
              [ gistFileName gistFile .=
                object ["content" .= gistFileText gistFile]
              ]
          ]
  request <-
    parseRequest $
    T.unpack $ "https://api.github.com/gists/" <> gistFileId gistFile
  -- TODO: GitHub API errors are not logged properly
  _ <-
    httpLbs
      (request
         { method = "PATCH"
         , requestBody = RequestBodyLBS $ encode payload
         , requestHeaders =
             ("User-Agent", encodeUtf8 "KGBotka") :
             ("Authorization", encodeUtf8 $ "token " <> token) :
             requestHeaders request
         })
      manager
  return ()

renderFridayVideo :: FridayVideo -> T.Text
renderFridayVideo video =
  T.pack $
  printf
    "|%s|%s|%s|"
    (show $ fridayVideoSubTime video)
    (fridayVideoAuthorDisplayName video)
    (fridayVideoSubText video)

renderQueue :: [FridayVideo] -> T.Text
renderQueue [] = ""
renderQueue videos@(FridayVideo {fridayVideoAuthorDisplayName = name}:_) =
  T.unlines $
  [ "** " <> name
  , ""
  , T.pack $ printf "Video Count: %d" $ length videos
  , ""
  , "|Date|Submitter|Video|"
  , "|-"
  ] <>
  map renderFridayVideo videos <>
  [""]

-- TODO(#130): renderAllQueues does not render thumbnails of the videos
renderAllQueues :: [[FridayVideo]] -> T.Text
renderAllQueues allQueues = header <> T.concat (map renderQueue allQueues)
  where
    header :: T.Text
    header =
      T.unlines
        [ "* Friday Queue"
        , ""
        , "Use ~!friday~ command to put a video here (only for trusted and subs)."
        , "*Any video can be skipped if the streamer finds it boring.*"
        , ""
        ]

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
import Control.Exception
import Data.Functor

data GithubThreadParams = GithubThreadParams
  { gtpSqliteConnection :: !(MVar Sqlite.Connection)
  , gtpManager :: !Manager
  , gtpLogQueue :: !(WriteQueue LogEntry)
  , gtpConfig :: !(Maybe ConfigGithub)
  , gtpUpdateRequired :: MVar ()
  }

instance ProvidesLogging GithubThreadParams where
  logQueue = gtpLogQueue

data GithubThreadState = GithubThreadState
  { gtsSqliteConnection :: !(MVar Sqlite.Connection)
  , gtsManager :: !Manager
  , gtsLogQueue :: !(WriteQueue LogEntry)
  , gtsConfig :: ConfigGithub
  , gtsUpdateRequired :: MVar ()
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
      , gtsUpdateRequired = gtpUpdateRequired gtp
      }
githubThread gtp =
  logEntry gtp $
  LogEntry "GITHUB" "[ERROR] GitHub configuration is not provided"

githubThreadLoop :: GithubThreadState -> IO ()
githubThreadLoop gts = do
  threadDelay $ 60 * 1000 * 1000
  takeMVar (gtsUpdateRequired gts)
  logEntry gts $ LogEntry "GITHUB" "Trying to update Friday Video Queue gist..."
  maybeFridayGistFile <-
    withMVar (gtsSqliteConnection gts) $ \conn ->
      Sqlite.withTransaction conn $
      runMaybeT $ do
        gistId <- MaybeT (settingsFridayGithubGistId <$> fetchSettings conn)
        gistText <- lift $ renderAllQueues <$> fetchAllQueues conn
        return $
          GistFile
            { gistFileId = gistId
            , gistFileText = gistText
            , gistFileName = "Friday.org"
            }
  case maybeFridayGistFile of
    Just fridayGistFile -> do
      logEntry gts $ LogEntry "GITHUB" "Updating Friday Video Queue gist..."
      updateGistFile
        gts
        (gtsManager gts)
        (configGithubToken $ gtsConfig gts)
        fridayGistFile
    Nothing ->
      logEntry gts $
      LogEntry
        "GITHUB"
        "fridayGithubGistId is not set in the settings. Nothing to update."
  githubThreadLoop gts

data GistFile = GistFile
  { gistFileId :: T.Text
  , gistFileName :: T.Text
  , gistFileText :: T.Text
  }

updateGistFile ::
     ProvidesLogging log => log -> Manager -> GithubToken -> GistFile -> IO ()
updateGistFile logger manager (GithubToken token) gistFile = do
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
  -- TODO(#133): GitHub API errors are not logged properly
  catch
    (void $
     httpLbs
       (request
          { method = "PATCH"
          , requestBody = RequestBodyLBS $ encode payload
          , requestHeaders =
              ("User-Agent", encodeUtf8 "KGBotka") :
              ("Authorization", encodeUtf8 $ "token " <> token) :
              requestHeaders request
          })
       manager)
    (\e ->
       logEntry logger $ LogEntry "GITHUB" $ T.pack $ show (e :: SomeException))
  return ()

renderFridayVideo :: FridayVideo -> T.Text
renderFridayVideo video =
  T.pack $
  printf
    "|%s|%s|%s|%s|"
    (show $ fridayVideoSubTime video)
    (fridayVideoAuthorDisplayName video)
    (fridayVideoSubText video)
    (either
       (const "")
       (\ytId -> "[[https://img.youtube.com/vi/" <> ytId <> "/default.jpg]]") $
     ytLinkId $ fridayVideoSubText video)

renderQueue :: [FridayVideo] -> T.Text
renderQueue [] = ""
renderQueue videos@(FridayVideo {fridayVideoAuthorDisplayName = name}:_) =
  T.unlines $
  [ "** " <> name
  , ""
  , T.pack $ printf "Video Count: %d" $ length videos
  , ""
  , "|Date|Submitter|Video|Thumbnail|"
  , "|-"
  ] <>
  map renderFridayVideo videos <>
  [""]

-- TODO(#130): renderAllQueues does not render thumbnails of the videos
renderAllQueues :: [[FridayVideo]] -> T.Text
renderAllQueues allQueues = header <> body
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
    body :: T.Text
    body =
      case allQueues of
        [] -> "No videos were submitted"
        _ -> T.concat (map renderQueue allQueues)

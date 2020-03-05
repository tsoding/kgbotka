{-# LANGUAGE OverloadedStrings #-}
module KGBotka.DiscordThread
  ( DiscordThreadParams(..)
  , discordThread
  ) where

import KGBotka.Config
import KGBotka.Queue
import KGBotka.Log
import Discord
import Discord.Types
import Control.Concurrent.STM
import qualified Discord.Requests as R
import qualified Data.Text as T
import Control.Monad

data DiscordThreadParams = DiscordThreadParams
  { dtpConfig :: Maybe ConfigDiscord
  , dtpLogQueue :: WriteQueue LogEntry
  }

discordThread :: DiscordThreadParams -> IO ()
discordThread dtp =
  case dtpConfig dtp of
    Just config -> do
      userFacingError <-
        runDiscord $
        def
          { discordToken = configDiscordToken config
          , discordOnEvent = eventHandler
          }
      atomically $
        writeQueue (dtpLogQueue dtp) $ LogEntry "DISCORD" userFacingError
    Nothing ->
      atomically $
      writeQueue (dtpLogQueue dtp) $
      LogEntry "DISCORD" "[ERROR] Discord configuration not found"

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis (MessageCreate m)
  | not (fromBot m) && isPing (messageText m) =
    void $
    restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
eventHandler _ _ = pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower

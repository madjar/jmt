{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Slack
import Web.Slack.Message
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import Data.Foldable
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens

import Reply

myConfig :: String -> SlackConfig
myConfig apiToken = SlackConfig
         { _slackApiToken = apiToken -- Specify your API token here
         }


main :: IO ()
main = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set")
               <$> lookupEnv "SLACK_API_TOKEN"
  args <- getArgs
  scripts <- mapM getLines args

  let opts = defaults & param "token" .~ [T.pack apiToken]
  r <- getWith opts "https://slack.com/api/channels.list"
  let generalId' = r ^? responseBody . key "channels"
                                     . values
                                     . filtered (\c -> c ^. key "name" . _String == "general")
                                     . key "id" . _String
      generalId = maybe (error "#general not found") id generalId'

      bot = filterBot (\i -> view getId i /= generalId) $ textBot (reply scripts)

  runBot (myConfig apiToken) bot ()

reply :: [Script] -> Text -> Maybe Text
reply scripts query = asum . map (getReply query) $ scripts

textBot :: (Text -> Maybe Text) -> SlackBot ()
textBot f (Message cid _ msg _ _ _)
  | (Just response) <- f msg = sendMessage cid response
textBot _ _ = return ()

-- | A bot middleware that filters channels based on the ChannelId
filterBot :: (ChannelId -> Bool) -> SlackBot a -> SlackBot a
filterBot f _ (Message cid _ _ _ _ _) | not (f cid) = return ()
filterBot _ b m = b m

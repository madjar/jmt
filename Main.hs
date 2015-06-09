module Main where

import Web.Slack
import Web.Slack.Message
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Text (Text)
import System.Environment
import Data.Foldable

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
  let bot = textBot $ reply scripts
  runBot (myConfig apiToken) bot ()

reply :: [Script] -> Text -> Maybe Text
reply scripts query = asum . map (getReply query) $ scripts

textBot :: (Text -> Maybe Text) -> SlackBot ()
textBot f (Message cid _ msg _ _ _)
  | (Just response) <- f msg = sendMessage cid response
textBot _ _ = return ()
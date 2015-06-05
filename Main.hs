module Main where

import Web.Slack
import Web.Slack.Message
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Text (Text)
import System.Environment

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
  lines <- concat <$> mapM getLines args
  let replyFunction query = getReply query lines
  runBot (myConfig apiToken) (textBot replyFunction) ()


textBot :: (Text -> Maybe Text) -> SlackBot ()
textBot f (Message cid _ msg _ _ _)
  | (Just response) <- f msg = sendMessage cid response
textBot _ _ = return ()
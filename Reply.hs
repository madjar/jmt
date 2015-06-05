{-# LANGUAGE OverloadedStrings #-}
module Reply where

import Text.Subtitles.SRT
import Data.Attoparsec.Text
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding
import Data.Either.Combinators
import Text.EditDistance
import Data.Maybe

rawLines :: FilePath -> IO [T.Text]
rawLines file = map dialog . fromRight' .  parseOnly parseSRT . decodeLatin1 <$> B.readFile file

cleanLines :: [Text] -> [Text]
cleanLines = map cleanup . filter notGarbage
  where notGarbage l = not ("--" `T.isPrefixOf` l || "<i>" `T.isPrefixOf` l)
        cleanup = T.replace "\n" " "

getLines s = cleanLines <$> rawLines s


searchLine query = filter (query `T.isInfixOf`)

-- |Search a line among a list of line and returns the list of matches, and their replies
searchReply :: Text -> [Text] -> [(Text, Text)]
searchReply query lines = filter ((< 5) . distance query . fst) (zip lines (tail lines))
  where distance t1 t2 = levenshteinDistance defaultEditCosts (T.unpack t1) (T.unpack t2)

getReply :: Text -> [Text] -> Maybe Text
getReply q = if T.length q > 5
               then fmap snd . listToMaybe . searchReply q
               else const Nothing
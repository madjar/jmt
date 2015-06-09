{-# LANGUAGE OverloadedStrings #-}
module Reply where

import Text.Subtitles.SRT
import Data.Attoparsec.Text (parseOnly)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Encoding
import Data.Either.Combinators
import Text.EditDistance
import Data.Maybe

type Script = [Text]

-- Try to read the text as a srt file, otherwise consider it as a script
readScript :: Text -> Script
readScript t = fromRight (parseScript t) (readSRT t)
  where readSRT = fmap (map dialog) . parseOnly parseSRT
        parseScript = filter (not . T.null) . map (T.strip . T.intercalate ":" . drop 1 . T.splitOn ":") . T.lines

readFileWithUnknownEncoding :: FilePath -> IO Text
readFileWithUnknownEncoding f = do bytes <- B.readFile f
                                   return $ fromRight (decodeLatin1 bytes) (decodeUtf8' bytes)

cleanScript :: Script -> Script
cleanScript = map cleanup . filter notGarbage
  where notGarbage l = not ("--" `T.isPrefixOf` l || "<i>" `T.isPrefixOf` l)
        cleanup = T.replace "\n" " "

getLines :: FilePath -> IO Script
getLines s = cleanScript . readScript <$> readFileWithUnknownEncoding s

searchLine :: Text -> Script -> [Text]
searchLine query = filter (query `T.isInfixOf`)

-- |Search a line among a list of line and returns the list of matches, and their replies
searchReply :: Text -> Script -> [(Text, Text)]
searchReply query lines = filter ((< 5) . distance query . fst) (zip lines (tail lines))
  where distance t1 t2 = levenshteinDistance defaultEditCosts (T.unpack t1) (T.unpack t2)

getReply :: Text -> [Text] -> Maybe Text
getReply q = if T.length q > 5
               then fmap snd . listToMaybe . searchReply q
               else const Nothing
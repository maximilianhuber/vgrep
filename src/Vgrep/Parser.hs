module Vgrep.Parser
    ( parseGrepOutput
    , parseLine
    , FileLineReference -- reexport from Vgrep.Results
    ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Maybe
import Data.Text

import Vgrep.Results

parseGrepOutput :: [Text] -> [FileLineReference]
parseGrepOutput = catMaybes . fmap parseLine

parseLine :: Text -> Maybe FileLineReference
parseLine line = maybeResult (parse lineParser line)

lineParser :: Parser FileLineReference
lineParser = do
    file       <- manyTill anyChar (char ':')
    lineNumber <- optional (decimal <* char ':')
    result     <- takeText
    return (File (pack file), (lineNumber, result))

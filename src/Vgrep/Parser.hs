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
parseLine line = case parseOnly lineParser line of
    Left  _      -> Nothing
    Right result -> Just result

lineParser :: Parser FileLineReference
lineParser = do
    file       <- manyTill anyChar (char ':')
    lineNumber <- optional (decimal <* char ':')
    result     <- takeText
    return (File (pack file), (lineNumber, result))

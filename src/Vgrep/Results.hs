module Vgrep.Results
    ( File(..)
    , LineReference
    , FileLineReference
    ) where

import           Data.Text (Text)


newtype File = File { getFileName :: Text } deriving (Eq)

type LineReference = (Maybe Int, Text)

type FileLineReference = (File, LineReference)

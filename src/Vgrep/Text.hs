{-# LANGUAGE FlexibleContexts #-}
module Vgrep.Text
    ( expandForDisplay
    , expandLineForDisplay
    ) where

import           Control.Lens
import           Control.Monad.Reader.Class
import           Data.Char
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T

import Vgrep.Environment


expandForDisplay :: MonadReader Environment m => [Text] -> m [Text]
expandForDisplay inputLines = do
    tabWidth <- view (config . tabstop)
    pure (map (expandText tabWidth) inputLines)

expandLineForDisplay :: MonadReader Environment m => Text -> m Text
expandLineForDisplay inputLine = do
    tabWidth <- view (config . tabstop)
    pure (expandText tabWidth inputLine)

expandText :: Int -> Text -> Text
expandText tabWidth =
    T.pack . expandSpecialChars . expandTabs tabWidth . T.unpack

expandTabs :: Int -> String -> String
expandTabs tabWidth = go 0
  where go pos (c:cs)
            | c == '\t' = let shift = tabWidth - (pos `mod` tabWidth)
                          in  replicate shift ' ' ++ go (pos + shift) cs
            | otherwise = c : go (pos + 1) cs
        go _ [] = []

expandSpecialChars :: String -> String
expandSpecialChars = \case
    c:cs | ord c < 32 -> ['^', chr (ord c + 64)] ++ expandSpecialChars cs
         | otherwise  -> c : expandSpecialChars cs
    []                -> []

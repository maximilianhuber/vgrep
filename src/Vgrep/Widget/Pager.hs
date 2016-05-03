{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Widget.Pager
    ( PagerState ()
    , PagerWidget
    , pagerWidget

    , moveToLine
    , scroll
    , scrollPage
    , replaceBufferContents
    ) where

import Control.Lens
import Control.Monad.State.Extended (put, modify)
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty.Image hiding (resize)
import Graphics.Vty.Input
import Graphics.Vty.Prelude

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type
import Vgrep.Widget.Type


data PagerState = PagerState
    { _position    :: Int
    , _highlighted :: Set Int
    , _above       :: [Text]
    , _visible     :: [Text] }

makeLensesFor [ ("_position", "position")
              , ("_visible", "visible")
              , ("_highlighted", "highlighted") ] ''PagerState

type PagerWidget = Widget PagerState

pagerWidget :: PagerWidget
pagerWidget =
    Widget { initialize = initPager
           , draw       = renderPager
           , handle     = fmap const pagerKeyBindings }

initPager :: PagerState
initPager = PagerState
    { _position    = 1
    , _highlighted = S.empty
    , _above       = []
    , _visible     = [] }


pagerKeyBindings
    :: Monad m
    => Event
    -> Next (VgrepT PagerState m Redraw)
pagerKeyBindings = dispatchMap $ fromList
    [ (EvKey KUp         [], scroll (-1)    )
    , (EvKey KDown       [], scroll 1       )
    , (EvKey (KChar 'k') [], scroll (-1)    )
    , (EvKey (KChar 'j') [], scroll 1       )
    , (EvKey KPageUp     [], scrollPage (-1))
    , (EvKey KPageDown   [], scrollPage 1   )
    ]

replaceBufferContents :: Monad m => [Text] -> [Int] -> VgrepT PagerState m ()
replaceBufferContents newContent newHighlightedLines = put $
    initPager { _visible     = newContent
              , _highlighted = S.fromList newHighlightedLines }

moveToLine :: Monad m => Int -> VgrepT PagerState m Redraw
moveToLine n = view region >>= \displayRegion -> do
    let height = regionHeight displayRegion
    pos <- use position
    scroll (n - height `div` 2 - pos)

scroll :: Monad m => Int -> VgrepT PagerState m Redraw
scroll n = view region >>= \displayRegion -> do
    let height = regionHeight displayRegion
    linesVisible <- uses visible (length . take (height + 1))
    if | n > 0 && linesVisible > height
                   -> modify goDown >> scroll (n - 1)
       | n < 0     -> modify goUp   >> scroll (n + 1)
       | otherwise -> pure Redraw
  where
    goDown (PagerState l h as     (b:bs)) = PagerState (l + 1) h (b:as) bs
    goDown (PagerState l h as     [])     = PagerState l       h as     []
    goUp   (PagerState l h (a:as) bs)     = PagerState (l - 1) h as     (a:bs)
    goUp   (PagerState l h []     bs)     = PagerState l       h []     bs

scrollPage :: Monad m => Int -> VgrepT PagerState m Redraw
scrollPage n = view region >>= \displayRegion ->
    let height = regionHeight displayRegion
    in  scroll (n * (height - 1))
      -- gracefully leave one ^ line on the screen


renderPager :: Monad m => VgrepT PagerState m Image
renderPager = do
    textColor         <- view (config . colors . normal)
    textColorHl       <- view (config . colors . normalHl)
    lineNumberColor   <- view (config . colors . lineNumbers)
    lineNumberColorHl <- view (config . colors . lineNumbersHl)
    (width, height)   <- view region
    startPosition     <- use position
    visibleLines      <- use (visible . to (take height))
    highlightedLines  <- use highlighted

    let renderLine (num, txt) =
            let (numColor, txtColor) = if num `S.member` highlightedLines
                    then (lineNumberColorHl, textColorHl)
                    else (lineNumberColor,   textColor)
            in  ( string numColor (padWithSpace (show num))
                , string txtColor (padWithSpace (T.unpack txt)) )

        (renderedLineNumbers, renderedTextLines)
            = over both fold . unzip
            . map renderLine
            $ zip [startPosition..] visibleLines

    pure (resizeWidth width (renderedLineNumbers <|> renderedTextLines))

  where padWithSpace s = ' ' : s ++ " "

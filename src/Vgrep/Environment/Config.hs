{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment.Config where

import Control.Lens
import Data.Maybe
import Graphics.Vty.Image
import System.Environment


--------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------

data Config = Config
    { _colors  :: Colors
    -- ^ Color configuration

    , _tabstop :: Int
    -- ^ Tabstop width (default: 8)

    , _editor  :: String
    -- ^ Executable for @e@ key (default: environment variable @$EDITOR@,
    -- or @vi@ if @$EDITOR@ is not set)

    } deriving (Eq, Show)

data Colors = Colors
    { _lineNumbers   :: Attr
    -- ^ Line numbers (default: blue)

    , _lineNumbersHl :: Attr
    -- ^ Highlighted line numbers (default: bold blue)

    , _normal        :: Attr
    -- ^ Normal text (default: terminal default)

    , _normalHl      :: Attr
    -- ^ Highlighted text (default: bold)

    , _fileHeaders   :: Attr
    -- ^ File names in results view (default: terminal default color on
    -- green background)

    , _selected      :: Attr
    -- ^ Selected entry (default: terminal default, inverted)

    } deriving (Eq, Show)


--------------------------------------------------------------------------
-- * Auto-generated Lenses
--------------------------------------------------------------------------

makeLenses ''Config
makeLenses ''Colors


--------------------------------------------------------------------------
-- * Default Config
--------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
    { _colors = Colors
        { _lineNumbers   = defAttr `withForeColor` blue
        , _lineNumbersHl = defAttr `withForeColor` blue
                                   `withStyle` bold
        , _normal        = defAttr
        , _normalHl      = defAttr `withStyle` bold
        , _fileHeaders   = defAttr `withBackColor` green
        , _selected      = defAttr `withStyle` standout }
    , _tabstop = 8
    , _editor = "vi" }

withConfiguredEditor :: Config -> IO Config
withConfiguredEditor config = do
    let defaultEditor = view editor config
    configuredEditor <- lookupEnv "EDITOR"
    pure config { _editor = fromMaybe defaultEditor configuredEditor }

{-# LANGUAGE OverloadedStrings #-}

module Options where

import Prelude
import qualified Turtle as Tu

data Options = Options
  { optDebug :: Bool
  , optVerbose :: Bool
  , optInteractive :: Bool
  }
  deriving (Eq, Show)

parser :: Tu.Parser Options
parser =
  Options <$>
    Tu.switch "debug" 'd' "Print debug information to stderr" <*>
    Tu.switch "verbose" 'v' "Print verbose information to stdout" <*>
    Tu.switch "interactive" 'i' "Handle found items interactively"

parseOpts :: Tu.Shell Options
parseOpts = Tu.options "Reddup: Keep your computer tidy" parser

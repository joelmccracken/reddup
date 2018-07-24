{-# LANGUAGE OverloadedStrings #-}

module Options where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import qualified System.IO as SIO

import qualified Config as C
import qualified Trackable as Track

data Options = Options
  {
    _debug :: Bool
  }
  deriving (Eq, Show)

parser :: Tu.Parser Options
parser =
  Options <$>
    Tu.switch "debug" 'd' "Print debug information to stderr"

parseOpts :: Tu.Shell Options
parseOpts = Tu.options "Reddup: Keep your computer tidy" parser

debug :: Options -> T.Text -> Tu.Shell ()
debug opts txt =
  if _debug opts then
    Tu.liftIO $ SIO.putStrLn $ T.unpack txt
  else
    return ()

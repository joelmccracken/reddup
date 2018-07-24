{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu

import qualified Config as C
import qualified Trackable as Track
import qualified Options as O

extractConfig :: Either String C.Config -> Tu.Shell C.Config
extractConfig eitherConfig =
  let
    doDie :: String -> Tu.Shell C.Config
    doDie errorMsg = Tu.die ("error parsing config: " Tu.<> (T.pack errorMsg))
  in either doDie return eitherConfig

main :: IO ()
main = Tu.sh $ do
  opts <- O.parseOpts
  O.debug opts $ T.pack $ show opts
  eitherConfig <- C.loadConfig
  config <- extractConfig eitherConfig
  O.debug opts $ T.pack $ show opts
  Track.handleTrackables $ Track.configToTrackables config
  O.debug opts "done"

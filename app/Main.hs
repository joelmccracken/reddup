{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu

import qualified Config as C
import qualified Trackable as Track
import qualified Options as O
import qualified Reddup  as R
import Control.Monad.Reader
import Debug.Trace (trace)

extractConfig :: Either String C.Config -> Tu.Shell C.Config
extractConfig eitherConfig =
  let
    die :: String -> Tu.Shell C.Config
    die errorMsg = Tu.die ("error parsing config: " Tu.<> (T.pack errorMsg))
  in either die return eitherConfig

checkConfig' :: C.Config -> Tu.Shell C.ProcessedConfig
checkConfig' config =
  let
    checkResult = C.processConfig config
    die :: Tu.Text -> Tu.Shell C.ProcessedConfig
    die = Tu.die  . ("error in config: " <>)
  in either (die . C.configErrorsDisplay) return checkResult

main :: IO ()
main = Tu.sh $ do
  opts <- O.parseOpts
  eitherConfig <- trace "loading config" C.loadConfig
  configUnchecked <- trace "extracting config" extractConfig eitherConfig
  pconfig <- trace "checking config" checkConfig' configUnchecked
  let reddup = trace "constructing R.ReddupD" R.ReddupD pconfig opts
  trace "runReaderT" runReaderT doIt reddup

doIt :: R.Reddup ()
doIt = do
  ask >>= trace "ask to R.Reddup ()" (R.debug . T.pack . show)
  trackable <- trace "config to trackables" Track.configToTrackables
  Track.handleTrackable trackable
  R.debug "done"

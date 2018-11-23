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
  eitherConfig <- C.loadConfig
  configUnchecked <- extractConfig eitherConfig
  pconfig <- checkConfig' configUnchecked
  let reddup = R.Reddup pconfig opts
  runReaderT doIt reddup

doIt :: R.ReddupT ()
doIt = do
  reddup <- ask
  let pconfig = R.reddupConfig reddup
  let trackables = Track.configToTrackables pconfig
  R.debug $ T.pack $ show reddup
  Track.handleTrackables trackables
  R.debug "done"

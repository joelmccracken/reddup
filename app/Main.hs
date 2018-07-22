{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import qualified System.IO as SIO

import qualified Config as C
import qualified Trackable as Track

data Command = IncreaseVolume Int | DecreaseVolume Int deriving (Show)

data Options = Options
  {
    _debug :: Bool
  }
  deriving (Eq, Show)

parser :: Tu.Parser Options
parser =
  Options <$>
    Tu.switch "debug" 'd' "Print debug information to stderr"

    --   fmap IncreaseVolume
    --     (Tu.subcommand "up" "Turn the volume up"
    --        (Tu.argInt "amount" "How much to increase the volume") )
    -- Tu.<|>
    --   fmap DecreaseVolume
    --     (Tu.subcommand "down" "Turn the volume down"
    --        (Tu.argInt "amount" "How much to decrease the volume") )

-- parser :: Tu.Parser Command
-- parser =
--       fmap IncreaseVolume
--         (Tu.subcommand "up" "Turn the volume up"
--            (Tu.argInt "amount" "How much to increase the volume") )
--     Tu.<|>
--       fmap DecreaseVolume
--         (Tu.subcommand "down" "Turn the volume down"
--            (Tu.argInt "amount" "How much to decrease the volume") )

-- main = do
--   x <-
--   case x of
--     IncreaseVolume n -> Tu.printf ("Increasing the volume by "%d%"\n") n
--     DecreaseVolume n -> Tu.printf ("Decreasing the volume by "%d%"\n") n


extractConfig :: Either String C.Config -> Tu.Shell C.Config
extractConfig eitherConfig =
  let
    doDie :: String -> Tu.Shell C.Config
    doDie errorMsg = Tu.die ("error parsing config: " Tu.<> (T.pack errorMsg))
  in either doDie return eitherConfig

main :: IO ()
main = Tu.sh $ do
  opts <- Tu.options "Reddup: Clean up your computer" parser
  Tu.liftIO $ putStrLn $ show opts
  eitherConfig <- C.loadConfig
  config <- extractConfig eitherConfig
  Tu.liftIO $ SIO.putStrLn $ show config
  Track.handleTrackables $ Track.configToTrackables config
  Tu.liftIO $ SIO.putStrLn "done"

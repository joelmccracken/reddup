{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Control.Applicative
import Data.Text hiding (empty)
import qualified Turtle as Tu
import Data.ByteString as BS
import qualified Data.Text as T
import qualified System.IO as SIO
import qualified ShellUtil

import Prelude -- Ensure Applicative is in scope and we have no warnings, before/after AMP.

data Config =
  Config {
    locations :: [LocationSpec]
  } deriving (Eq, Show)

data LocationSpec =
  LocationSpec {
    _type    :: Text
  , location :: Text
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .:   "locations"
  parseJSON _ = fail "error parsing"

instance FromJSON LocationSpec where
  parseJSON (Y.Object v) =
    LocationSpec <$>
    v .:   "type"   <*>
    v .:   "location"
  parseJSON _ = fail "error parsing"

getConfigFilename :: Tu.Shell SIO.FilePath
getConfigFilename = fmap (Tu.fromString . T.unpack . Tu.lineToText) (ShellUtil.expandGlob "~/.reddup.yml")

loadConfig :: Tu.Shell (Either String Config)
loadConfig = do
  configFilename <- getConfigFilename
  configContents <- Tu.liftIO $ (BS.readFile configFilename :: IO BS.ByteString)
  return ((Y.decodeEither configContents) :: Either String Config)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Config where

-- import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Control.Applicative
import Data.Text hiding (empty)


import Text.RawString.QQ
import Data.ByteString (ByteString)
import Prelude -- Ensure Applicative is in scope and we have no warnings, before/after AMP.

configYaml :: ByteString
configYaml = [r|
locations:
  - type: git
    location: ~/ttm/*
  - type: git
    location: ~/EF
  - type: git
    location: ~/Reference
  - type: git
    location: ~/Projects/*
  - type: inbox
    location: ~/Desktop
  - type: inbox
    location: ~/Inbox
|]

data Config =
  MkConfig {
    locations :: [LocationSpec]
  } deriving (Eq, Show)

data LocationSpec =
  MkLocationSpec {
    _type    :: Text
  , location :: Text
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    MkConfig <$>
    v .:   "locations"
  parseJSON _ = fail "error parsing"

instance FromJSON LocationSpec where
  parseJSON (Y.Object v) =
    MkLocationSpec <$>
    v .:   "type"   <*>
    v .:   "location"
  parseJSON _ = fail "error parsing"

loadConfig :: IO (Either String Config)
loadConfig = do
  let decoded = (Y.decodeEither configYaml) :: Either String Config
  return decoded

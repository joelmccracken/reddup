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
  Config
    { locations :: [LocationSpec]
    , handlers :: HandlerSpecs
    } deriving (Eq, Show)

data LocationSpec =
  LocationSpec
    { _type    :: Text
    , location :: Text
    } deriving (Eq, Show)

data HandlerSpecs =
  HandlerSpecs
    { inboxHandlers :: InboxHandlerSpec
    } deriving (Eq, Show)

data InboxHandlerSpec =
  InboxHandlerSpec
    { commands :: [InboxHandlerCommandSpec]
    } deriving (Eq, Show)


data InboxHandlerCommandSpec =
  InboxHandlerCommandSpec
    { name :: Text
    , cmd  :: Text
    } deriving (Eq, Show)


instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
      v .:   "locations" <*>
      v .:   "handlers"
  parseJSON _ = fail "error parsing config"

instance FromJSON LocationSpec where
  parseJSON (Y.Object v) =
    LocationSpec <$>
    v .:   "type"   <*>
    v .:   "location"
  parseJSON _ = fail "error parsing location specs"

instance FromJSON HandlerSpecs where
  parseJSON (Y.Object v) =
    HandlerSpecs <$>
    v .: "inbox"
  parseJSON _ = fail "error parsing handler specs"

instance FromJSON InboxHandlerSpec where
  parseJSON (Y.Object v) =
    InboxHandlerSpec <$>
      v .: "commands"
  parseJSON _ = fail "error parsing inbox handler spec"

instance FromJSON InboxHandlerCommandSpec where
  parseJSON (Y.Object v) =
    InboxHandlerCommandSpec <$>
      v .: "name" <*>
      v .: "cmd"
  parseJSON _ = fail "error parsing inbox handler command"

getConfigFilename :: Tu.Shell SIO.FilePath
getConfigFilename = fmap (Tu.fromString . T.unpack . Tu.lineToText) (ShellUtil.expandGlob "~/.reddup.yml")

loadConfig :: Tu.Shell (Either String Config)
loadConfig = do
  configFilename <- getConfigFilename
  configContents <- Tu.liftIO $ (BS.readFile configFilename :: IO BS.ByteString)
  return ((Y.decodeEither configContents) :: Either String Config)

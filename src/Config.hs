{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))
-- import Control.Applicative
import Data.Text hiding (empty)
import qualified Turtle as Tu
import Data.ByteString as BS
import qualified Data.Text as T
import qualified System.IO as SIO
import qualified ShellUtil
import qualified Data.Map.Strict as M
import qualified Data.List as List

data ProcessedConfig =
  ProcessedConfig
    { rawConfig :: Config
    , inboxHandlerCommands :: CustomHandlers
    , inboxRefileDests :: RefileDests
    } deriving (Eq, Show)

type CustomHandlers = M.Map T.Text InboxHandlerCommandSpec

type RefileDests = M.Map T.Text InboxHandlerRefileDestsSpec

data Config =
  Config
    { locations :: [LocationSpec]
    , handlers :: HandlerSpecs
    } deriving (Eq, Show)

data LocationSpec
  = GitLoc {location :: Text}
  | InboxLoc {location :: Text, ignoredFiles :: Maybe [Text] }
  deriving (Eq, Show)

data HandlerSpecs =
  HandlerSpecs
    { inboxHandlers :: InboxHandlerSpec
    } deriving (Eq, Show)

data InboxHandlerSpec =
  InboxHandlerSpec
    { commands :: Maybe [InboxHandlerCommandSpec]
    , refileDests :: Maybe [InboxHandlerRefileDestsSpec]
    } deriving (Eq, Show)

data InboxHandlerCommandSpec =
  InboxHandlerCommandSpec
    { cmdName :: Text
    , cmdSpecCmd :: Text
    , cmdKey :: Text
    } deriving (Eq, Show)

data InboxHandlerRefileDestsSpec =
  InboxHandlerRefileDestsSpec
    { mvDestName :: Text
    , dirChar :: Text
    , mvDestDir :: Text
    } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
      v .:   "locations" <*>
      v .:   "handlers"
  parseJSON _ = fail "error parsing config"

instance FromJSON LocationSpec where
  parseJSON = Y.withObject "LocationSpec" $ \v -> do
    type'     <- v .:  "type"
    location' <- v .:  "location"
    ignoredFiles' <- v .:? "ignored_files"
    case (T.unpack $ type') of
      "git" -> return $ GitLoc location'
      "inbox" -> return $ InboxLoc location' ignoredFiles'
      _ -> fail $ "Location type must be either 'git' or 'inbox', found '" <> T.unpack type' <> "'"

instance FromJSON HandlerSpecs where
  parseJSON (Y.Object v) =
    HandlerSpecs <$>
    v .: "inbox"
  parseJSON _ = fail "error parsing handler specs"

instance FromJSON InboxHandlerSpec where
  parseJSON (Y.Object v) =
    InboxHandlerSpec <$>
      v .:? "commands" <*>
      v .:? "refile_dests"
  parseJSON _ = fail "error parsing inbox handler spec"

instance FromJSON InboxHandlerCommandSpec where
  parseJSON (Y.Object v) =
    InboxHandlerCommandSpec <$>
      v .: "name" <*>
      v .: "cmd" <*>
      v .: "key"
  parseJSON _ = fail "error parsing inbox handler command"

instance FromJSON InboxHandlerRefileDestsSpec where
  parseJSON (Y.Object v) =
    InboxHandlerRefileDestsSpec <$>
      v .: "name" <*>
      v .: "char" <*>
      v .: "dir"
  parseJSON _ = fail "error parsing inbox handler command"

getConfigFilename :: Tu.Shell SIO.FilePath
getConfigFilename = fmap (Tu.fromString . T.unpack . Tu.lineToText) (ShellUtil.expandGlob "~/.reddup.yml")

loadConfig :: Tu.Shell (Either String Config)
loadConfig = do
  configFilename <- getConfigFilename
  configContents <- Tu.liftIO $ (BS.readFile configFilename :: IO BS.ByteString)
  -- TODO change this to decodeEither'
  return ((Y.decodeEither configContents) :: Either String Config)

data ConfigError
  = CharKeyWrongNumCharsError InboxHandlerCommandSpec
  deriving (Eq, Show)

processInboxCommandHandlersConfig ::
  Maybe [InboxHandlerCommandSpec] ->
  ([ConfigError], CustomHandlers)
processInboxCommandHandlersConfig maybeCmdSpecs =
  let
    cmdSpecs :: [InboxHandlerCommandSpec]
    cmdSpecs = maybe [] id maybeCmdSpecs

    hasRightNumChars spec = (List.length $ T.unpack $ (cmdKey spec) ) > 0

    (rightNumCharsCmds,
     wrongNumCharsCmds) = List.partition hasRightNumChars cmdSpecs

    errors =
      (CharKeyWrongNumCharsError <$> wrongNumCharsCmds)

    toPair spec = (cmdKey spec, spec)

    successes = toPair <$> rightNumCharsCmds
  in
    (errors, M.fromList successes)

processConfig :: Config -> Either [ConfigError] ProcessedConfig
processConfig config =
  let
    inboxHandlerCommands' :: Maybe [InboxHandlerCommandSpec]
    inboxHandlerCommands' = commands $ inboxHandlers $ handlers config
    (ihcErrors, ihcSuccesses) = processInboxCommandHandlersConfig inboxHandlerCommands'

    inboxRefileDests' :: Maybe [InboxHandlerRefileDestsSpec]
    inboxRefileDests' = refileDests $ inboxHandlers $ handlers config
    refileDests' = processRefileDests inboxRefileDests'

    allErrors = ihcErrors

    newConfig = ProcessedConfig
      { rawConfig = config
      , inboxHandlerCommands = ihcSuccesses
      , inboxRefileDests = M.empty
      }
  in
    if List.length allErrors > 0 then
      Left allErrors
    else
      Right newConfig


processRefileDests :: Maybe [InboxHandlerRefileDestsSpec] -> RefileDests
processRefileDests refileDestSpecs = undefined

configErrorsDisplay :: [ConfigError] -> Text
configErrorsDisplay ce =
  foldMap configErrorDisplay ce

configErrorDisplay :: ConfigError -> Text
configErrorDisplay ce =
  case ce of
    CharKeyWrongNumCharsError handlerSpec ->
      let
        cmdName' = T.pack $ show $ cmdName handlerSpec
        cmdKey' = T.pack $ show $ cmdKey handlerSpec
      in
        "key for command " <>
        cmdName' <>
        ", key value is " <>
        cmdKey' <>
        ", key must be at least one character long.\n"

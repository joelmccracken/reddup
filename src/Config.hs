{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Maybe (fromMaybe)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))
import Data.Text hiding (empty)
import qualified Turtle as Tu
import Data.ByteString as BS
import qualified Data.Text as T
import qualified System.IO as SIO
import qualified ShellUtil
import qualified Data.Map.Strict as M
import qualified Data.List as List
import qualified Data.Bifunctor as BF

data ProcessedConfig =
  ProcessedConfig
    { rawConfig :: Config
    , inboxHandlerCommands :: CustomHandlers
    , inboxRefileDests :: RefileDests
    } deriving (Eq, Show)

type CustomHandlers = M.Map T.Text InboxHandlerCommandSpec

type RefileDests = M.Map T.Text InboxHandlerRefileDestSpec

data Config =
  Config
    { locations :: [LocationSpec]
    , handlers :: HandlerSpecs
    } deriving (Eq, Show)

data LocationSpec
  = GitLoc GitLocation
  | InboxLoc InboxLocation
  deriving (Eq, Show)

data GitLocation = GitLocation
    { gitLocation :: Text
    , gitForce    :: Bool
    , gitMessage  :: !Text
    }
  deriving (Eq, Show)

data InboxLocation = InboxLocation
    { inboxLocation :: Text
    , ignoredFiles :: [Text]
    }
  deriving (Eq, Show)

data HandlerSpecs =
  HandlerSpecs
    { inboxHandlers :: InboxHandlerSpec
    } deriving (Eq, Show)

data InboxHandlerSpec =
  InboxHandlerSpec
    { commands :: Maybe [InboxHandlerCommandSpec]
    , refileDests :: Maybe [InboxHandlerRefileDestSpec]
    } deriving (Eq, Show)

data InboxHandlerCommandSpec =
  InboxHandlerCommandSpec
    { cmdName :: Text
    , cmdSpecCmd :: Text
    , cmdKey :: Text
    } deriving (Eq, Show)

data InboxHandlerRefileDestSpec =
  InboxHandlerRefileDestSpec
    { refileDestName :: Text
    , refileDestKey :: Text
    , refileDestDir :: Text
    } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
      v .:   "locations" <*>
      v .:   "handlers"
  parseJSON _ = fail "error parsing config"

instance FromJSON LocationSpec where
  parseJSON = Y.withObject "LocationSpec" $ \v -> do
    type'         <- v .:  "type"
    location'     <- v .:  "location"
    force         <- v .:? "force"
    ignoredFiles' <- v .:? "ignored_files"
    maybeMsg      <- v .:? "commit_message"
    case (T.unpack $ type') of
      "git" -> return $ GitLoc $ GitLocation location' (fromMaybe False force) (fromMaybe "WIP" maybeMsg)
      "inbox" -> return $ InboxLoc $ InboxLocation location' (fromMaybe [] ignoredFiles')
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

instance FromJSON InboxHandlerRefileDestSpec where
  parseJSON (Y.Object v) =
    InboxHandlerRefileDestSpec <$>
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
  let eresults = Y.decodeEither' configContents :: Either Y.ParseException Config
  return (BF.first Y.prettyPrintParseException eresults)

data ConfigError
  = ErrorCmdHandlerKeyWrongNumChars InboxHandlerCommandSpec
  | ErrorRefileDestKeyWrongNumChars InboxHandlerRefileDestSpec
  deriving (Eq, Show)

processInboxCommandHandlers ::
  Maybe [InboxHandlerCommandSpec] ->
  ([ConfigError], CustomHandlers)
processInboxCommandHandlers maybeCmdSpecs =
  let
    cmdSpecs :: [InboxHandlerCommandSpec]
    cmdSpecs = maybe [] id maybeCmdSpecs

    hasRightNumChars spec = (List.length $ T.unpack $ (cmdKey spec) ) > 0

    (rightNumCharsCmds,
     wrongNumCharsCmds) = List.partition hasRightNumChars cmdSpecs

    errors =
      (ErrorCmdHandlerKeyWrongNumChars <$> wrongNumCharsCmds)

    toPair spec = (cmdKey spec, spec)

    successes = toPair <$> rightNumCharsCmds
  in
    (errors, M.fromList successes)

processConfig :: Config -> Either [ConfigError] ProcessedConfig
processConfig config =
  let
    inboxHandlerCommands' :: Maybe [InboxHandlerCommandSpec]
    inboxHandlerCommands' = commands $ inboxHandlers $ handlers config
    (ihcErrors, ihcSuccesses) = processInboxCommandHandlers inboxHandlerCommands'

    inboxRefileDests' :: Maybe [InboxHandlerRefileDestSpec]
    inboxRefileDests' = refileDests $ inboxHandlers $ handlers config
    (refileErrors, refileDests') = processRefileDests inboxRefileDests'

    allErrors = ihcErrors ++ refileErrors

    newConfig = ProcessedConfig
      { rawConfig = config
      , inboxHandlerCommands = ihcSuccesses
      , inboxRefileDests = refileDests'
      }
  in
    if List.length allErrors > 0 then
      Left allErrors
    else
      Right newConfig

moreThanOneChar :: String -> Bool
moreThanOneChar str =
  List.length str > 0

processRefileDests :: Maybe [InboxHandlerRefileDestSpec] -> ([ConfigError], RefileDests)
processRefileDests maybeRefileDestSpecs =
  let
    refileSpecs :: [InboxHandlerRefileDestSpec]
    refileSpecs = maybe [] id maybeRefileDestSpecs

    hasRightNumChars :: InboxHandlerRefileDestSpec -> Bool
    hasRightNumChars spec = moreThanOneChar $ T.unpack $ refileDestKey spec

    (rightNumCharsCmds,
     wrongNumCharsCmds) = List.partition hasRightNumChars refileSpecs

    errors = ErrorRefileDestKeyWrongNumChars <$> wrongNumCharsCmds

    toPair spec = (refileDestKey spec, spec)

    successes = toPair <$> rightNumCharsCmds
  in
    (errors, M.fromList successes)

configErrorsDisplay :: [ConfigError] -> Text
configErrorsDisplay ce =
  foldMap configErrorDisplay ce

configErrorDisplay :: ConfigError -> Text
configErrorDisplay ce =
  case ce of
    ErrorCmdHandlerKeyWrongNumChars handlerSpec ->
      let
        cmdName' = T.pack $ show $ cmdName handlerSpec
        cmdKey' = T.pack $ show $ cmdKey handlerSpec
      in
        "key for command " <>
        cmdName' <>
        ", key value is " <>
        cmdKey' <>
        ", key must be at least one character long.\n"
    ErrorRefileDestKeyWrongNumChars handlerSpec ->
      let
        name = T.pack $ show $ refileDestName handlerSpec
        key = T.pack $ show $ refileDestKey handlerSpec
      in
        "key for refile destination " <>
        name <>
        ", key value is " <>
        key <>
        ", key must be at least one character long."

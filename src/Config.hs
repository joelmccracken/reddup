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
    , gitHandlerCommands :: !CustomGitHandlers
    } deriving (Eq, Show)

type CustomHandlers    = M.Map T.Text InboxHandlerCommandSpec
-- / key is the letter of command
type CustomGitHandlers = M.Map T.Text GitHandlerCommandSpec
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
    }
  deriving (Eq, Show)

data InboxLocation = InboxLocation
    { inboxLocation :: Text
    , ignoredFiles :: [Text]
    }
  deriving (Eq, Show)

data HandlerSpecs
  = InboxSpec
      { inboxHandlers :: InboxHandlerSpec
      }
  | GitSpec
      { gitHandlers :: GitHandlerSpec
      }
  deriving (Eq, Show)

newtype GitHandlerSpec
  = GitHandlerSpec {gitCommands :: Maybe [GitHandlerCommandSpec]}
  deriving (Eq, Show)

data GitHandlerCommandSpec =
  GitHandlerCommandSpec
    { gitCmdName    :: !Text
    , gitCmdSpecCmd :: !Text
    , gitCmdKey     :: !Text
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
    type'     <- v .:  "type"
    location' <- v .:  "location"
    force     <- v .:? "force"
    ignoredFiles' <- v .:? "ignored_files"
    case (T.unpack $ type') of
      "git" -> return $ GitLoc $ GitLocation location' (fromMaybe False force)
      "inbox" -> return $ InboxLoc $ InboxLocation location' (fromMaybe [] ignoredFiles')
      _ -> fail $ "Location type must be either 'git' or 'inbox', found '" <> T.unpack type' <> "'"


instance FromJSON HandlerSpecs where
  parseJSON = Y.withObject "HandlerSpecs" $ \v -> do
    inbox' <- v .:? "inbox"
    git'   <- v .:? "git"
    case inbox' of
      Just val -> return $ InboxSpec val
      Nothing -> case git' of
                   Just gitVal -> return $ GitSpec gitVal
                   Nothing -> fail "error parsing handler specs"

instance FromJSON InboxHandlerSpec where
  parseJSON (Y.Object v) =
    InboxHandlerSpec <$>
      v .:? "commands" <*>
      v .:? "refile_dests"
  parseJSON _ = fail "error parsing inbox handler spec"

instance FromJSON GitHandlerSpec where
  parseJSON (Y.Object v) =
    GitHandlerSpec <$>
      v .:? "commands"
  parseJSON _ = fail "error parsing git handler spec"

instance FromJSON GitHandlerCommandSpec where
  parseJSON (Y.Object v) =
    GitHandlerCommandSpec <$>
      v .: "name" <*>
      v .: "cmd"  <*>
      v .: "key"
  parseJSON _ = fail "error parsing git handler command"

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
  | ErrorGitCmdHandlerKeyWrongNumChars GitHandlerCommandSpec
  deriving (Eq, Show)

processGitCommandHandlers ::
  Maybe [GitHandlerCommandSpec] ->
  ([ConfigError], CustomGitHandlers)
processGitCommandHandlers maybeCmdSpecs = 
  let
    cmdSpecs :: [GitHandlerCommandSpec]
    cmdSpecs = fromMaybe [] maybeCmdSpecs
    
    hasRightNumChars spec = (List.length $ T.unpack $ (gitCmdKey spec) ) > 0
    
    (rightNumCharsCmds,
         wrongNumCharsCmds) = List.partition hasRightNumChars cmdSpecs
    
    errors =
      (ErrorGitCmdHandlerKeyWrongNumChars <$> wrongNumCharsCmds) 
    
    toPair spec = (gitCmdKey spec, spec)
    
    successes = toPair <$> rightNumCharsCmds
  in
    (errors, M.fromList successes)      
         


processInboxCommandHandlers ::
  Maybe [InboxHandlerCommandSpec] ->
  ([ConfigError], CustomHandlers)
processInboxCommandHandlers maybeCmdSpecs =
  let
    cmdSpecs :: [InboxHandlerCommandSpec]
    cmdSpecs = fromMaybe [] maybeCmdSpecs

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
    --inboxHandlerCommands' = commands $ inboxHandlers $ handlers config
    inboxHandlerCommands' = 
      case handlers config of
        InboxSpec handlers' -> commands handlers'
        _                   -> Nothing
    
    (ihcErrors, ihcSuccesses) = processInboxCommandHandlers inboxHandlerCommands' 

    inboxRefileDests' :: Maybe [InboxHandlerRefileDestSpec]
    inboxRefileDests' = refileDests $ inboxHandlers $ handlers config
    (refileErrors, refileDests') = processRefileDests inboxRefileDests'

    gitHandlerCommands' :: Maybe [GitHandlerCommandSpec]
    --gitHandlerCommands' = gitCommands $ gitHandlers $ handlers config
    gitHandlerCommands' =
      case handlers config of
        GitSpec handlers' -> gitCommands handlers'
        _                 -> Nothing  
    
    (ghcErrors, ghcSuccesses) = processGitCommandHandlers gitHandlerCommands'
    
    allErrors = ihcErrors ++ refileErrors ++ ghcErrors

    newConfig = ProcessedConfig
      { rawConfig = config
      , inboxHandlerCommands = ihcSuccesses
      , inboxRefileDests = refileDests'
      , gitHandlerCommands = ghcSuccesses
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
    ErrorGitCmdHandlerKeyWrongNumChars handlerSpec ->
      let
        cmdName' = T.pack $ show $ gitCmdName handlerSpec
        cmdKey' = T.pack $ show $ gitCmdKey handlerSpec
      in
        "key for command " <>
        cmdName' <>
        ", key value is " <>
        cmdKey' <>
        ", key must be at least one character long.\n"   

{-# LANGUAGE OverloadedStrings #-}

module Trackable where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import qualified Git as Git
-- import qualified System.IO as SIO
import Data.Monoid ((<>))
import qualified GitParse as GP
import qualified ShellUtil
import qualified Config as C
import qualified Options as O

type GitRepoPath = Tu.FilePath
type InboxPath = Tu.FilePath
type FilePath = Tu.FilePath

data Trackable
  = GitRepo GitRepoPath
  | InboxDir InboxPath
  | UnknownTrackable T.Text Tu.FilePath
  deriving (Show)

data NeedsHandled
  = NHGit GitRepoPath NHGit
  | NHFile InboxPath FilePath

data NHGit
  = NHStatus GP.GitStatus
  | NHUnpushedBranch GP.GitBranch
  | NHNotGitRepo

printHandler :: NeedsHandled -> Tu.Shell ()
printHandler nh =
  Tu.liftIO $ putStrLn $ T.unpack $ format
  where
    format =
      case nh of
        NHGit dir' nhg ->
          let dir = pathToTextOrError dir' in
          case nhg of
            NHStatus (GP.Staged f) -> formatPath dir f "staged changes"
            NHStatus (GP.Unstaged f) -> formatPath dir f "unstaged changes"
            NHStatus (GP.StagedAndUnstaged f) -> formatPath dir f "staged and unstaged changes"
            NHStatus (GP.Untracked f) -> formatPath dir f "untracked file"
            NHStatus (GP.Unknown f) -> formatPath dir f "(unknown git status)"
            NHStatus (GP.Deleted f) -> formatPath dir f "file deleted"
            NHUnpushedBranch (GP.GitBranch branchName) ->
              dir <> ": Unpushed branch '" <> branchName <> "'"
            NHNotGitRepo -> dir <> ": is not a git repo"
        NHFile inbox file ->
          (pathToTextOrError inbox) <> ": file present " <> (pathToTextOrError file)

    formatPath :: T.Text -> T.Text -> T.Text -> T.Text
    formatPath path statusItem label =
      path <> ": " <> label <> " '" <> statusItem  <> "'"

handleTrackables :: Tu.Shell Trackable -> O.Options -> Tu.Shell ()
handleTrackables trackables opts =
  trackables >>= handleTrackable opts

handleTrackable :: O.Options -> Trackable -> Tu.Shell ()
handleTrackable opts trackable =
  case trackable of
    (GitRepo repo) ->
      handleGitTrackable opts repo >>= printHandler
    (InboxDir dir) ->
      handleInboxTrackable opts dir >>= printHandler
    (UnknownTrackable type' dir) ->
      handleUnknownTrackable opts type' dir

handleGitTrackable :: O.Options -> GitRepoPath -> Tu.Shell NeedsHandled
handleGitTrackable opts dir = do
  O.verbose opts $ "checking " <> (T.pack $ show dir)
  Tu.cd dir
  dirExists <- Tu.testdir ".git"
  if dirExists then
    checkGitStatus dir
  else
    return $ NHGit dir NHNotGitRepo

handleUnknownTrackable :: O.Options -> T.Text -> Tu.FilePath -> Tu.Shell ()
handleUnknownTrackable  _ type' dir =
  Tu.liftIO $ print $ ("warning: encountered unknown trackable definition " Tu.<> type' Tu.<> " at " Tu.<> (T.pack (show dir)))

handleInboxTrackable :: O.Options -> Tu.FilePath -> Tu.Shell NeedsHandled
handleInboxTrackable opts dir = do
  O.verbose opts $ "checking " <> pathToTextOrError dir
  Tu.cd dir
  let status = Tu.ls "."
  status >>= (return . NHFile dir)

formatInboxTrackable :: Tu.FilePath -> Tu.FilePath -> T.Text
formatInboxTrackable dir item =
  (pathToTextOrError dir) <> "/" <> (pathToTextOrError item) <> ": file present"

checkGitStatus :: GitRepoPath -> Tu.Shell NeedsHandled
checkGitStatus repo = do
  (wrapUnpushed Git.unpushedGitBranches) Tu.<|>
    (wrapStatus Git.gitStatus)
  where
    wrapStatus   = (NHGit repo <$> NHStatus <$>)
    wrapUnpushed = (NHGit repo <$> NHUnpushedBranch <$>)

pathToTextOrError ::  Tu.FilePath -> T.Text
pathToTextOrError path =
  case (Tu.toText path) of
    Left l ->
      "(Decode ERROR: problem with path encoding for " <> l <> ")"
    Right r -> r

configToTrackables :: C.Config -> Tu.Shell Trackable
configToTrackables (C.Config { C.locations = locations }) = do
  location <- Tu.select locations
  locationSpecToTrackable location

locationSpecToTrackable :: C.LocationSpec -> Tu.Shell Trackable
locationSpecToTrackable (C.LocationSpec { C._type = _type,  C.location = location }) = do
  let allExpanded = fmap (Tu.fromText . Tu.lineToText) (ShellUtil.expandGlob location)
  _path <- allExpanded
  let trackable =
        case _type of
          "git"   -> GitRepo _path
          "inbox" -> InboxDir _path
          _       -> UnknownTrackable _type  _path
  return trackable

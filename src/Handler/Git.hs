{-# LANGUAGE OverloadedStrings #-}

module Handler.Git where

import qualified Data.Text as T
import Trackable.Data
import Trackable.Util
import qualified GitParse as GP
import qualified Turtle as Tu
import qualified Reddup  as R
import Control.Monad.Reader (ask, lift, runReaderT, liftIO)
import qualified Git

gitHandler :: NHGit -> R.Reddup ()
gitHandler nh@(NHGit (GitRepoTrackable dir' _locSpec) nhg) = do
  isInteractive <- R.isInteractive
  if isInteractive then
    lift $ gitPrintHandler nh
  else
    lift $ gitPrintHandler nh

gitPrintHandler :: NHGit -> Tu.Shell ()
gitPrintHandler (NHGit (GitRepoTrackable dir' _locSpec) nhg) =
  Tu.liftIO $ putStrLn $ T.unpack $ format
  where
    format =
      let dir = pathToTextOrError dir' in
      case nhg of
        NHStatus (GP.Added f) -> formatPath dir f "file added"
        NHStatus (GP.AddedAndModified f) -> formatPath dir f "file added and modified"
        NHStatus (GP.Staged f) -> formatPath dir f "staged changes"
        NHStatus (GP.Unstaged f) -> formatPath dir f "unstaged changes"
        NHStatus (GP.StagedAndUnstaged f) -> formatPath dir f "staged and unstaged changes"
        NHStatus (GP.Untracked f) -> formatPath dir f "untracked file"
        NHStatus (GP.Deleted f) -> formatPath dir f "file deleted"
        NHStatus (GP.Unknown f) -> formatPath dir f "(unknown git status)"
        NHUnpushedBranch (GP.GitBranch branchName) ->
          dir <> ": Unpushed branch '" <> branchName <> "'"
        NHNotGitRepo -> dir <> ": is not a git repo"
    formatPath :: T.Text -> T.Text -> T.Text -> T.Text
    formatPath path statusItem label =
      path <> ": " <> label <> " '" <> statusItem  <> "'"

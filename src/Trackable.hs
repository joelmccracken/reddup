{-# LANGUAGE OverloadedStrings #-}

module Trackable where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import qualified Git as Git
-- import qualified System.IO as SIO
import Data.Monoid ((<>))
import qualified ShellUtil
import qualified Config as C
import qualified Handler as H
import qualified Reddup  as R
import Trackable.Data
import Trackable.Util
import Control.Monad.Reader

handleTrackables :: Tu.Shell Trackable -> ReaderT R.Reddup Tu.Shell ()
handleTrackables trackables = do
  (lift trackables) >>= handleTrackable

handleTrackable :: Trackable -> ReaderT R.Reddup Tu.Shell ()
handleTrackable trackable = do
  case trackable of
    (GitRepo repo) ->
      handleGitTrackable repo >>= lift . H.gitPrintHandler
    (InboxDir dir) ->
      handleInboxTrackable dir >>= handleInbox

handleInbox ::  NHFile -> R.ReddupT ()
handleInbox nh = do
  isInteractive <- R.isInteractive
  if isInteractive then
    H.inboxInteractiveHandler nh
  else
    lift $ H.inboxPrintHandler nh

handleGitTrackable :: GitRepoPath -> ReaderT R.Reddup Tu.Shell NHGit
handleGitTrackable dir = do
  R.verbose $ "checking " <> (T.pack $ show dir)
  lift $ Tu.cd dir
  dirExists <- lift $ Tu.testdir ".git"
  if dirExists then
    lift $ checkGitStatus dir
  else
    lift $ return $ NHGit dir NHNotGitRepo

handleInboxTrackable :: Tu.FilePath -> R.ReddupT NHFile
handleInboxTrackable dir = do
  R.verbose $ "checking " <> pathToTextOrError dir
  lift $ Tu.cd dir
  let files = lift $ Tu.ls dir
  files >>= (lift . return . NHFile dir)

formatInboxTrackable :: Tu.FilePath -> Tu.FilePath -> T.Text
formatInboxTrackable dir item =
  (pathToTextOrError dir) <> "/" <> (pathToTextOrError item) <> ": file present"

checkGitStatus :: GitRepoPath -> Tu.Shell NHGit
checkGitStatus repo = do
  (wrapUnpushed Git.unpushedGitBranches) Tu.<|>
    (wrapStatus Git.gitStatus)
  where
    wrapStatus   = (NHGit repo <$> NHStatus <$>)
    wrapUnpushed = (NHGit repo <$> NHUnpushedBranch <$>)

configToTrackables :: C.ProcessedConfig -> Tu.Shell Trackable
configToTrackables conf = do
  location <- Tu.select $ C.locations $ C.rawConfig conf
  locationSpecToTrackable location

locationSpecToTrackable :: C.LocationSpec -> Tu.Shell Trackable
locationSpecToTrackable ls = do
  let expand location =
        (Tu.fromText . Tu.lineToText) <$> (ShellUtil.expandGlob location)

  case ls of
    C.GitLoc location -> do
      path' <- (expand location)
      return $ GitRepo path'

    C.InboxLoc location _foo -> do
      path' <- (expand location)
      return $ InboxDir path'

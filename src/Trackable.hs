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
import qualified Options as O
import qualified Handler as H
import qualified Reddup  as R
import Trackable.Data
import Trackable.Util
import Control.Monad.Reader

handleTrackables :: Tu.Shell Trackable -> ReaderT R.Reddup Tu.Shell ()
handleTrackables trackables = do
  reddup <- ask
  let config = R.reddupConfig reddup
  let opts    = R.reddupOptions reddup
  trackable <- lift trackables
  lift $ handleTrackable opts config trackable

handleTrackable :: O.Options -> C.ProcessedConfig -> Trackable -> Tu.Shell ()
handleTrackable opts config trackable =
  case trackable of
    (GitRepo repo) ->
      handleGitTrackable opts repo >>= H.gitPrintHandler
    (InboxDir dir) ->
      handleInboxTrackable opts dir >>= handleInbox opts config
    (UnknownTrackable type' dir) ->
      handleUnknownTrackable opts type' dir

handleInbox :: O.Options -> C.ProcessedConfig -> NHFile -> Tu.Shell ()
handleInbox opts config nh =
  if O._interactive opts then
    H.inboxInteractiveHandler nh config
  else
    H.inboxPrintHandler nh

handleGitTrackable :: O.Options -> GitRepoPath -> Tu.Shell NHGit
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

handleInboxTrackable :: O.Options -> Tu.FilePath -> Tu.Shell NHFile
handleInboxTrackable opts dir = do
  O.verbose opts $ "checking " <> pathToTextOrError dir
  Tu.cd dir
  let files = Tu.ls dir
  files >>= (return . NHFile dir)

  -- i want to try to convert this path here to text and err out if it does not work
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

{-# LANGUAGE OverloadedStrings #-}

module Trackable where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import qualified Git
-- import qualified System.IO as SIO
import Data.Monoid ((<>))
import qualified ShellUtil
import qualified Config as C
import qualified Handler as H
import qualified Handler.Git as HG
import qualified Reddup  as R
import Trackable.Data
import Trackable.Util
import Control.Monad.Reader

handleTrackable :: Trackable -> R.Reddup ()
handleTrackable trackable = do
  case trackable of
    (GitRepo grTrack) ->
      HG.gitHandler' grTrack
    (InboxDir idTrack) ->
      processInboxTrackable idTrack >>= H.handleInbox

processInboxTrackable :: InboxDirTrackable -> R.Reddup NHFile
processInboxTrackable idt@(InboxDirTrackable dir _locSpec)= do
  R.verbose $ "checking " <> pathToTextOrError dir
  lift $ Tu.cd dir
  let files = lift $ Tu.ls dir
  files >>= (lift . return . NHFile idt)

formatInboxTrackable :: Tu.FilePath -> Tu.FilePath -> T.Text
formatInboxTrackable dir item =
  (pathToTextOrError dir) <> "/" <> (pathToTextOrError item) <> ": file present"

configToTrackables :: R.Reddup Trackable
configToTrackables = do
  reddup <- ask
  location <- lift $ Tu.select $ C.locations $ C.rawConfig $ R.reddupConfig reddup
  lift $ locationSpecToTrackable location

locationSpecToTrackable :: C.LocationSpec -> Tu.Shell Trackable
locationSpecToTrackable ls = do
  let expand location =
        (Tu.fromText . Tu.lineToText) <$> (ShellUtil.expandGlob location)
  case ls of
    C.GitLoc location -> do
      path' <- (expand location)
      return $ GitRepo $ GitRepoTrackable path' ls
    C.InboxLoc location _foo -> do
      path' <- (expand location)
      return $ InboxDir $ InboxDirTrackable path' ls

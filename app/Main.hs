{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import qualified Git as Git
import qualified GitParse as GitParse
import qualified Config as C
import qualified ShellUtil
import qualified System.IO as SIO

data Trackable
  = GitRepo  Tu.FilePath
  | InboxDir Tu.FilePath
  | UnknownTrackable T.Text Tu.FilePath
  deriving (Show)

gitDirectoriesConfig :: [T.Text] -> Tu.Shell Trackable
gitDirectoriesConfig dirs = do
  repoDir <- Tu.select dirs
  path <- ShellUtil.expandGlob repoDir
  return $ (GitRepo . Tu.fromText . Tu.lineToText $ path)

inboxDirectoriesConfig :: [T.Text] -> Tu.Shell Trackable
inboxDirectoriesConfig dirs = do
  repoDir <- Tu.select dirs
  path <- ShellUtil.expandGlob repoDir
  return $ (InboxDir . Tu.fromText . Tu.lineToText $ path)

handleTrackables :: Tu.Shell Trackable -> Tu.Shell ()
handleTrackables trackables = trackables >>= handleTrackable

handleTrackable :: Trackable -> Tu.Shell ()
handleTrackable (GitRepo dir) = do
  Tu.liftIO $ SIO.putStrLn $ "checking " ++ show dir
  Tu.cd dir
  dirExists <- Tu.testdir ".git"
  if dirExists then
    Tu.liftIO checkGitStatus
  else
    Tu.liftIO $ SIO.putStrLn $ "ERROR IS NOT GIT REPO"
  return ()

handleTrackable (UnknownTrackable _type dir) =
  Tu.liftIO $ print $ ("warning: encountered unknown trackable definition" Tu.<> _type Tu.<> "at" Tu.<> (T.pack (show dir)))

handleTrackable (InboxDir dir) = do
  Tu.liftIO $ SIO.putStrLn $ "checking " ++ show dir
  Tu.cd dir
  let status = Tu.ls "."
  Tu.view status
  return ()

checkGitStatus :: IO ()
checkGitStatus = do
  let status = Git.gitStatus
  Tu.view status
  let unpushedBranches = Git.unpushedGitBranches
  Tu.stdout $ fmap Tu.unsafeTextToLine $ fmap viewGitBranchAsUnpushed unpushedBranches

viewGitBranchAsUnpushed :: GitParse.GitBranchType -> T.Text
viewGitBranchAsUnpushed (GitParse.GitBranch name) =
  T.append "Unpushed: " name

configToTrackables :: C.Config -> Tu.Shell Trackable
configToTrackables (C.MkConfig { C.locations = locations }) = do
  location <- Tu.select locations
  locationSpecToTrackable location

locationSpecToTrackable :: C.LocationSpec -> Tu.Shell Trackable
locationSpecToTrackable (C.MkLocationSpec { C._type = _type,  C.location = location }) = do
  let allExpanded = fmap (Tu.fromText . Tu.lineToText) (ShellUtil.expandGlob location)
  _path <- allExpanded
  let trackable =
        case _type of
          "git"   -> GitRepo _path
          "inbox" -> InboxDir _path
          _       -> UnknownTrackable _type  _path
  return trackable

extractConfig :: Either String C.Config -> Tu.Shell C.Config
extractConfig eitherConfig =
  let
    doDie :: String -> Tu.Shell C.Config
    doDie errorMsg = Tu.die ("error parsing config: " Tu.<> (T.pack errorMsg))
  in either doDie return eitherConfig

main :: IO ()
main = Tu.sh $ do
  eitherConfig <- C.loadConfig
  config <- extractConfig eitherConfig
  Tu.liftIO $ SIO.putStrLn $ show config
  handleTrackables $ configToTrackables config
  Tu.liftIO $ SIO.putStrLn "done"

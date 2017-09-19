{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import qualified Git as Git
import qualified GitParse as GitParse
import Data.ByteString (readFile, ByteString)
import qualified Config
import qualified ShellUtil
import qualified System.IO as SIO

data Trackable
  = GitRepo  Tu.FilePath
  | InboxDir Tu.FilePath
  | UnknownTrackable T.Text Tu.FilePath
  deriving (Show)

directoriesConfig :: Tu.Shell Trackable
directoriesConfig = do
  let
    gitRepos = [ "~/EF"
               , "~/Reference"
               , "~/Projects/*"
               , "~/ttm/apangea"
               ]
    inboxDirs = [ "~/Inbox",
                  "~/Desktop"
                ]
    in do
      gitDirectoriesConfig gitRepos
        Tu.<|> inboxDirectoriesConfig inboxDirs

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
  Tu.liftIO $ putStrLn $ "checking " ++ show dir
  Tu.cd dir
  dirExists <- Tu.testdir ".git"
  if dirExists then
    Tu.liftIO checkGitStatus
  else
    Tu.liftIO $ putStrLn $ "ERROR IS NOT GIT REPO"
  return ()

handleTrackable (UnknownTrackable _type dir) =
  Tu.liftIO $ print $ ("warning: encountered unknown trackable definition" Tu.<> _type Tu.<> "at" Tu.<> (T.pack (show dir)))

handleTrackable (InboxDir dir) = do
  Tu.liftIO $ putStrLn $ "checking " ++ show dir
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

extractConfig :: Either String Config.Config -> Tu.Shell Config.Config
extractConfig eitherConfig =
  let
    doDie :: String -> Tu.Shell Config.Config
    doDie errorMsg = Tu.die ("error parsing config: " Tu.<> (T.pack errorMsg))
  in either doDie return eitherConfig

configToTrackables :: Config.Config -> Tu.Shell Trackable
configToTrackables (Config.MkConfig { Config.locations = locations }) = do
  location <- Tu.select locations
  locationSpecToTrackable location

locationSpecToTrackable :: Config.LocationSpec -> Tu.Shell Trackable
locationSpecToTrackable (Config.MkLocationSpec { Config._type = _type,  Config.location = location }) = do
  let allExpanded = fmap (Tu.fromText . Tu.lineToText) (ShellUtil.expandGlob location)
  _path <- allExpanded
  let trackable =
        case _type of
          "git"   -> GitRepo _path
          "inbox" -> InboxDir _path
          _       -> UnknownTrackable _type  _path
  return trackable

getConfigFilename :: Tu.Shell SIO.FilePath
getConfigFilename = fmap (Tu.fromString . T.unpack . Tu.lineToText) (ShellUtil.expandGlob "~/.reddup.yml")

main :: IO ()
main = Tu.sh $ do
  configFilename <- getConfigFilename
  configContents <- Tu.liftIO $ (Data.ByteString.readFile configFilename :: IO ByteString)
  eitherConfig <- Tu.liftIO $ Config.loadConfig configContents
  config <- extractConfig $ eitherConfig
  handleTrackables $ configToTrackables config
  Tu.liftIO $ putStrLn "done"

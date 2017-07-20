{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Text
import Prelude hiding (FilePath, concat)
import qualified Turtle as T
import qualified Git as Git
import qualified GitParse as GitParse
import Data.ByteString (readFile, ByteString)
import qualified Config
import qualified ShellUtil
import qualified System.IO as SIO

data Trackable
  = GitRepo  T.FilePath
  | InboxDir T.FilePath
  | UnknownTrackable Text.Text T.FilePath
  deriving (Show)

directoriesConfig :: T.Shell Trackable
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
        T.<|> inboxDirectoriesConfig inboxDirs

gitDirectoriesConfig :: [Text.Text] -> T.Shell Trackable
gitDirectoriesConfig dirs = do
  repoDir <- T.select dirs
  path <- ShellUtil.expandGlob repoDir
  return $ (GitRepo . T.fromText . T.lineToText $ path)

inboxDirectoriesConfig :: [Text.Text] -> T.Shell Trackable
inboxDirectoriesConfig dirs = do
  repoDir <- T.select dirs
  path <- ShellUtil.expandGlob repoDir
  return $ (InboxDir . T.fromText . T.lineToText $ path)

handleTrackables :: [Trackable] -> T.Shell ()
handleTrackables trackables = do
  Prelude.foldl (>>) (return ()) (fmap handleTrackable trackables)


handleTrackable :: Trackable -> T.Shell ()
handleTrackable (GitRepo dir) = do
  T.liftIO $ putStrLn $ "checking " ++ show dir
  T.cd dir
  dirExists <- T.testdir ".git"
  if dirExists then
    T.liftIO checkGitStatus
  else
    T.liftIO $ putStrLn $ "ERROR IS NOT GIT REPO"
  return ()

handleTrackable (UnknownTrackable _type dir) =
  T.liftIO $ print $ ("warning: encountered unknown trackable definition" T.<> _type T.<> "at" T.<> (Text.pack (show dir)))

handleTrackable (InboxDir dir) = do
  T.liftIO $ putStrLn $ "checking " ++ show dir
  T.cd dir
  let status = T.ls "."
  T.view status
  return ()

checkGitStatus :: IO ()
checkGitStatus = do
  let status = Git.gitStatus
  T.view status
  let unpushedBranches = Git.unpushedGitBranches
  T.stdout $ fmap T.unsafeTextToLine $ fmap viewGitBranchAsUnpushed unpushedBranches

viewGitBranchAsUnpushed :: GitParse.GitBranchType -> Text.Text
viewGitBranchAsUnpushed (GitParse.GitBranch name) =
  Text.append "Unpushed: " name

extractConfig :: Either String Config.Config -> T.Shell Config.Config
extractConfig eitherConfig =
  let
    doDie :: String -> T.Shell Config.Config
    doDie errorMsg = T.die ("error parsing config: " T.<> (Text.pack errorMsg))
  in either doDie return eitherConfig

configToTrackables :: Config.Config -> [Trackable]
configToTrackables (Config.MkConfig { Config.locations = locations }) =
  fmap locationSpecToTrackable locations

locationSpecToTrackable :: Config.LocationSpec -> Trackable
locationSpecToTrackable (Config.MkLocationSpec { Config._type = _type,  Config.location = location }) =
  case _type of
    "git"   -> GitRepo $ T.fromText location
    "inbox" -> InboxDir $ T.fromText location
    _       -> UnknownTrackable _type $ T.fromText location

getConfigFilename :: T.Shell SIO.FilePath
getConfigFilename = fmap (T.fromString . Text.unpack . T.lineToText) (ShellUtil.expandGlob "~/.reddup.yml")

main :: IO ()
main = T.sh $ do
  configFilename <- getConfigFilename
  configContents <- T.liftIO $ (Data.ByteString.readFile configFilename :: IO ByteString)
  eitherConfig <- T.liftIO $ Config.loadConfig configContents
  config <- extractConfig $ eitherConfig
  handleTrackables $ configToTrackables config
  T.liftIO $ putStrLn "done"

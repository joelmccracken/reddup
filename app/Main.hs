{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text hiding (empty)
import Prelude hiding (FilePath, concat)
import Turtle
import Git
import GitParse
import Data.ByteString (readFile, ByteString)
import qualified Config
import qualified ShellUtil
import qualified System.IO as SIO

data Trackable
  = GitRepo  Turtle.FilePath
  | InboxDir Turtle.FilePath
  | UnknownTrackable Text Turtle.FilePath
  deriving (Show)

directoriesConfig :: Shell Trackable
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
        <|> inboxDirectoriesConfig inboxDirs

gitDirectoriesConfig :: [Text] -> Shell Trackable
gitDirectoriesConfig dirs = do
  repoDir <- select dirs
  path <- ShellUtil.expandGlob repoDir
  return $ (GitRepo . fromText . lineToText $ path)

inboxDirectoriesConfig :: [Text] -> Shell Trackable
inboxDirectoriesConfig dirs = do
  repoDir <- select dirs
  path <- ShellUtil.expandGlob repoDir
  return $ (InboxDir . fromText . lineToText $ path)

handleTrackables :: [Trackable] -> Shell ()
handleTrackables trackables = do
  Prelude.foldl (>>) (return ()) (fmap handleTrackable trackables)


handleTrackable :: Trackable -> Shell ()
handleTrackable (GitRepo dir) = do
  liftIO $ putStrLn $ "checking " ++ show dir
  cd dir
  dirExists <- testdir ".git"
  if dirExists then
    liftIO checkGitStatus
  else
    liftIO $ putStrLn $ "ERROR IS NOT GIT REPO"
  return ()

handleTrackable (UnknownTrackable _type dir) =
  liftIO $ print $ ("warning: encountered unknown trackable definition" <> _type <> "at" <> (pack (show dir)))

handleTrackable (InboxDir dir) = do
  liftIO $ putStrLn $ "checking " ++ show dir
  cd dir
  let status = ls "."
  view status
  return ()

checkGitStatus :: IO ()
checkGitStatus = do
  let status = gitStatus
  view status
  let unpushedBranches = unpushedGitBranches
  stdout $ fmap unsafeTextToLine $ fmap viewGitBranchAsUnpushed unpushedBranches

viewGitBranchAsUnpushed :: GitBranchType -> Text
viewGitBranchAsUnpushed (GitBranch name) =
  Data.Text.append "Unpushed: " name

extractConfig :: Either String Config.Config -> Shell Config.Config
extractConfig eitherConfig =
  let
    doDie :: String -> Shell Config.Config
    doDie errorMsg = die ("error parsing config: " <> (pack errorMsg))
  in either doDie return eitherConfig

configToTrackables :: Config.Config -> [Trackable]
configToTrackables (Config.MkConfig { Config.locations = locations }) =
  fmap locationSpecToTrackable locations

locationSpecToTrackable :: Config.LocationSpec -> Trackable
locationSpecToTrackable (Config.MkLocationSpec { Config._type = _type,  Config.location = location }) =
  case _type of
    "git"   -> GitRepo $ fromText location
    "inbox" -> InboxDir $ fromText location
    _       -> UnknownTrackable _type $ fromText location

getConfigFilename :: Shell SIO.FilePath
getConfigFilename = fmap (fromString . unpack . lineToText) (ShellUtil.expandGlob "~/.reddup.yml")

main :: IO ()
main = sh $ do
  configFilename <- getConfigFilename
  configContents <- liftIO $ (Data.ByteString.readFile configFilename :: IO ByteString)
  eitherConfig <- liftIO $ Config.loadConfig configContents
  config <- extractConfig $ eitherConfig
  handleTrackables $ configToTrackables config
  liftIO $ putStrLn "done"

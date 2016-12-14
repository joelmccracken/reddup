{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import System.Directory
import Prelude hiding (FilePath)
import Turtle hiding ((<|>))
import Git

data Trackable
  = Git Turtle.FilePath
  | InboxDir Turtle.FilePath
  deriving (Show)

gitTrackables :: [String] -> [Trackable]
gitTrackables directories =
  Prelude.map gitTrackable directories

gitTrackable :: String -> Trackable
gitTrackable = Git . fromString


inboxTrackables :: [String] -> [Trackable]
inboxTrackables = Prelude.map inboxTrackable

inboxTrackable :: String -> Trackable
inboxTrackable = InboxDir . fromString

directoriesToCheck :: Shell [Trackable]
directoriesToCheck = do
  homeDir <- liftIO $ getHomeDirectory
  return $ directoriesConfig homeDir

directoriesConfig homeDir =
  let
    toHomeDir = Prelude.map (homeDir ++)
    gitRepos =
      toHomeDir [ "/EF/"
                , "/Reference/"
                , "/Projects/git-stuff/"
                ]
    inboxDirs =
      toHomeDir [ "/Inbox", "/Desktop" ]
  in
    gitTrackables gitRepos ++ inboxTrackables inboxDirs

handleTrackable :: Trackable -> Shell ()
handleTrackable (Git dir) = do
  liftIO $ putStrLn $ "checking " ++ show dir
  cd dir
  dirExists <- testdir ".git"
  if dirExists then
    liftIO checkGitStatus
  else
    liftIO $ putStrLn $ "ERROR IS NOT GIT REPO"
  return ()

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

main :: IO ()
main = sh (do
  trackables_ <- directoriesToCheck
  trackable <- select trackables_
  handleTrackable trackable
  )

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle hiding ((<|>))
import Data.Text
import System.Directory
import Prelude hiding (FilePath)
import Text.ParserCombinators.Parsec as Parsec

data GitStatusType
  = Staged Text
  | Unstaged Text
  | Untracked Text
  | Unknown Text
  deriving (Show)

data Trackable
  = Git Turtle.FilePath
  | InboxDir Turtle.FilePath
  deriving (Show)

gitStatusLineParser :: Parsec.Parser GitStatusType
gitStatusLineParser = do
  parseUntracked
    <|> parseStaged
    <|> parseUnstaged
    <|> parseUnknown

parseUntracked :: Parsec.Parser GitStatusType
parseUntracked = do
  _ <- string "??"
  value <- parseValueAfterLabel
  return $ Untracked (fromString value)

parseStaged :: Parsec.Parser GitStatusType
parseStaged = do
  _ <- string "M "
  value <- parseValueAfterLabel
  return $ Staged (fromString value)

parseUnstaged :: Parsec.Parser GitStatusType
parseUnstaged = do
  _ <- string " M"
  value <- parseValueAfterLabel
  return $ Unstaged (fromString value)

parseUnknown :: Parsec.Parser GitStatusType
parseUnknown = do
  wholeLine <- Parsec.many Parsec.anyChar
  return $ Unknown (fromString wholeLine)

parseValueAfterLabel :: Parsec.Parser String
parseValueAfterLabel = do
  _ <- Parsec.space
  Parsec.many Parsec.anyChar

parseGitStatusLine :: Turtle.Line -> Either ParseError GitStatusType
parseGitStatusLine line =
  parse gitStatusLineParser "git status --porcelain" ((unpack . lineToText) line)

gitStatus :: Shell (Either ParseError GitStatusType)
gitStatus = do
  let statusStream = inshell "git status --porcelain" Turtle.empty
  fmap parseGitStatusLine statusStream

trackables :: String -> [Trackable]
trackables homeDir =
  Prelude.map (Git . fromString)
    [ homeDir ++ "/EF/"
    , homeDir ++ "/Reference/"
    , homeDir ++ "/Projects/git-stuff/"
    ]

directoriesToCheck :: Shell [Trackable]
directoriesToCheck = do
  homeDir <- liftIO $ getHomeDirectory
  return $ trackables homeDir ++ [InboxDir ""]

handleTrackable :: Trackable -> Shell ()
handleTrackable (Git dir) = do
  liftIO $ putStrLn $ "checking " ++ show dir
  cd dir
  let status = gitStatus
  view status
  return ()

handleTrackable (InboxDir _) = do
  return ()

main :: IO ()
main = sh (do
  trackables_ <- directoriesToCheck
  trackable <- select trackables_
  handleTrackable trackable
  )

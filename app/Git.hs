{-# LANGUAGE OverloadedStrings #-}

module Git where

import Text.ParserCombinators.Parsec as Parsec
import Data.Text
import Turtle hiding ((<|>))
import Prelude hiding (FilePath)


data GitBranchType
  = GitBranch Text
  deriving (Show)

data GitStatusType
  = Staged Text
  | Unstaged Text
  | Untracked Text
  | Unknown Text
  deriving (Show)

gitStatusLineParser :: Parsec.Parser GitStatusType
gitStatusLineParser = do
  parseUntracked
    <|> parseStaged
    <|> parseUnstaged

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

parseValueAfterLabel :: Parsec.Parser String
parseValueAfterLabel = do
  _ <- Parsec.space
  Parsec.many Parsec.anyChar

parseGitStatusLine :: Turtle.Line -> GitStatusType
parseGitStatusLine line =
  let
    parsed = parse gitStatusLineParser "git status --porcelain" ((unpack . lineToText) line)
  in
    case parsed of
      Left _ -> Unknown (lineToText line)
      Right value -> value

gitBranchLineParser :: Parsec.Parser GitBranchType
gitBranchLineParser =
  let
    spacesThenAnything = (Parsec.spaces >> Parsec.many Parsec.anyChar)
    branchNameParser = (string "*" >> spacesThenAnything)
             <|> spacesThenAnything
  in do
    name <- branchNameParser
    return $ GitBranch (fromString name)

parseGitBranchLine :: Turtle.Line -> GitBranchType
parseGitBranchLine line =
  let
    parsed = parse gitBranchLineParser "git branch" ((unpack . lineToText) line)
  in
    case parsed of
      Left _ -> GitBranch (lineToText line)
      Right value -> value

gitStatus :: Shell GitStatusType
gitStatus = do
  let statusStream = inshell "git status --porcelain" Turtle.empty
  fmap parseGitStatusLine statusStream

gitBranches :: Shell GitBranchType
gitBranches = do
  let branchStream = inshell "git branch" Turtle.empty
  fmap parseGitBranchLine branchStream

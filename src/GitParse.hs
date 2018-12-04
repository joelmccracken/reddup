module GitParse where

import Data.Text
import Text.ParserCombinators.Parsec as Parsec
import Turtle hiding ((<|>))

newtype GitBranch
  = GitBranch Text
  deriving (Show, Eq)

data GitStatus
  = Added Text
  | AddedAndModified Text
  | Staged Text
  | StagedAndUnstaged Text
  | Unstaged Text
  | Untracked Text
  | Deleted Text
  | Unknown Text
  deriving (Show, Eq)

gitStatusLineParser :: Parsec.Parser GitStatus
gitStatusLineParser = do
  try parseUntracked
    <|> try parseAdded
    <|> try parseAddedAndModified
    <|> try parseStaged
    <|> try parseStagedAndUnstaged
    <|> try parseDeleted
    <|> try parseUnstaged

parseUntracked :: Parsec.Parser GitStatus
parseUntracked = do
  _ <- string "??"
  value <- parseValueAfterLabel
  return $ Untracked (fromString value)

parseAdded :: Parsec.Parser GitStatus
parseAdded = do
  _ <- string "A "
  value <- parseValueAfterLabel
  return $ Added (fromString value)

parseAddedAndModified :: Parsec.Parser GitStatus
parseAddedAndModified = do
  _ <- string "AM"
  value <- parseValueAfterLabel
  return $ AddedAndModified (fromString value)

parseStaged :: Parsec.Parser GitStatus
parseStaged = do
  _ <- string "M "
  value <- parseValueAfterLabel
  return $ Staged (fromString value)

parseStagedAndUnstaged :: Parsec.Parser GitStatus
parseStagedAndUnstaged = do
  _ <- string "MM"
  value <- parseValueAfterLabel
  return $ StagedAndUnstaged (fromString value)

parseDeleted :: Parsec.Parser GitStatus
parseDeleted = do
  _ <- string " D"
  value <- parseValueAfterLabel
  return $ Deleted (fromString value)

parseUnstaged :: Parsec.Parser GitStatus
parseUnstaged = do
  _ <- string " M"
  value <- parseValueAfterLabel
  return $ Unstaged (fromString value)

parseValueAfterLabel :: Parsec.Parser String
parseValueAfterLabel = do
  _ <- Parsec.space
  Parsec.many Parsec.anyChar

parseGitStatusLine :: Turtle.Line -> GitStatus
parseGitStatusLine line =
  let
    parsed = parse gitStatusLineParser "git status --porcelain" ((unpack . lineToText) line)
  in
    case parsed of
      Left _ -> Unknown (lineToText line)
      Right value -> value

gitBranchLineParser :: Parsec.Parser GitBranch
gitBranchLineParser =
  let
    spacesThenAnything = (Parsec.spaces >> Parsec.many Parsec.anyChar)
    branchNameParser = (string "*" >> spacesThenAnything)
             <|> spacesThenAnything
  in do
    name <- branchNameParser
    return $ GitBranch (fromString name)

parseGitBranchLine :: Turtle.Line -> GitBranch
parseGitBranchLine line =
  let
    parsed = parse gitBranchLineParser "git branch" ((unpack . lineToText) line)
  in
    case parsed of
      Left _ -> GitBranch (lineToText line)
      Right value -> value

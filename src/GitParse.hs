module GitParse ( parseGitBranchLine
                , parseGitStatusLine
                , GitStatus(..)
                , GitBranch(..)
                ) where

import Prelude (Show, Eq, Either(..), String, ($), return, (>>), (.))

import Data.Text
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), try)
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

gitStatusLineParser :: P.Parser GitStatus
gitStatusLineParser = do
  try parseUntracked
    <|> try parseAdded
    <|> try parseAddedAndModified
    <|> try parseStaged
    <|> try parseStagedAndUnstaged
    <|> try parseDeleted
    <|> try parseUnstaged

parseUntracked :: P.Parser GitStatus
parseUntracked = do
  _ <- P.string "??"
  value <- parseValueAfterLabel
  return $ Untracked (fromString value)

parseAdded :: P.Parser GitStatus
parseAdded = do
  _ <- P.string "A "
  value <- parseValueAfterLabel
  return $ Added (fromString value)

parseAddedAndModified :: P.Parser GitStatus
parseAddedAndModified = do
  _ <- P.string "AM"
  value <- parseValueAfterLabel
  return $ AddedAndModified (fromString value)

parseStaged :: P.Parser GitStatus
parseStaged = do
  _ <- P.string "M "
  value <- parseValueAfterLabel
  return $ Staged (fromString value)

parseStagedAndUnstaged :: P.Parser GitStatus
parseStagedAndUnstaged = do
  _ <- P.string "MM"
  value <- parseValueAfterLabel
  return $ StagedAndUnstaged (fromString value)

parseDeleted :: P.Parser GitStatus
parseDeleted = do
  _ <- P.string " D"
  value <- parseValueAfterLabel
  return $ Deleted (fromString value)

parseUnstaged :: P.Parser GitStatus
parseUnstaged = do
  _ <- P.string " M"
  value <- parseValueAfterLabel
  return $ Unstaged (fromString value)

parseValueAfterLabel :: P.Parser String
parseValueAfterLabel = do
  _ <- P.space
  P.many P.anyChar

parseGitStatusLine :: Turtle.Line -> GitStatus
parseGitStatusLine line =
  let
    parsed = P.parse gitStatusLineParser "git status --porcelain" ((unpack . lineToText) line)
  in
    case parsed of
      Left _ -> Unknown (lineToText line)
      Right value -> value

gitBranchLineParser :: P.Parser GitBranch
gitBranchLineParser =
  let
    spacesThenAnything = (P.spaces >> P.many P.anyChar)
    branchNameParser :: P.Parser String
    branchNameParser = (P.string "*" >> spacesThenAnything)
             <|> spacesThenAnything
  in do
    name <- branchNameParser
    return $ GitBranch (fromString name)

parseGitBranchLine :: Turtle.Line -> GitBranch
parseGitBranchLine line =
  let
    parsed = P.parse gitBranchLineParser "git branch" ((unpack . lineToText) line)
  in
    case parsed of
      Left _ -> GitBranch (lineToText line)
      Right value -> value

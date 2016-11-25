{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Turtle hiding ((<|>))
import Data.Text
import System.Directory
import System.Posix.IO
import Prelude hiding (FilePath)
import System.IO
import Text.ParserCombinators.Parsec as Parsec

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
    <|> parseUnknown

parseUntracked :: Parsec.Parser GitStatusType
parseUntracked = do
  string "??"
  value <- parseValueAfterLabel
  return $ Untracked (fromString value)

parseStaged = do
  string "M "
  value <- parseValueAfterLabel
  return $ Staged (fromString value)

parseUnstaged = do
  string " M"
  value <- parseValueAfterLabel
  return $ Unstaged (fromString value)

parseUnknown = do
  wholeLine <- Parsec.many Parsec.anyChar
  return $ Unknown (fromString wholeLine)

parseValueAfterLabel = do
  Parsec.space
  Parsec.many Parsec.anyChar

parseGitStatusLine :: Turtle.Line -> Either ParseError GitStatusType
parseGitStatusLine line =
  parse gitStatusLineParser "git status --porcelain" ((unpack . lineToText) line)

gitStatus :: Shell (Either ParseError GitStatusType)
gitStatus = do
  let statusStream = inshell "git status --porcelain" Turtle.empty
  fmap parseGitStatusLine statusStream

trackables :: String -> [Turtle.FilePath]
trackables home =
  Prelude.map fromString
    [ home ++ "/EF/"
    , home ++ "/Reference/"
    , home ++ "/Projects/git-stuff/"
    ]

directoriesToCheck :: Shell [Turtle.FilePath]
directoriesToCheck = do
  home <- liftIO $ getHomeDirectory
  return $ trackables home

main :: IO ()
main = sh (do
  dirs <- directoriesToCheck
  dir <- select dirs
  liftIO $ putStrLn $ "checking " ++ show dir
  cd dir
  let status = gitStatus
  view status
  return ())

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Turtle
import Data.Text
import System.Directory
import System.Posix.IO
import Prelude hiding (FilePath)
import System.IO

-- parser :: Parser (FilePath, FilePath)
-- parser = (,) <$> argPath "src" "the source file"
--              <*> argPath "dest" "the destination file"


data BackupConfigOption =
  Directory String
  deriving (Show, Read)


main :: IO ()
main = do
  -- (src, dest) <- options "a simple `cp` script" parser
  home <- getHomeDirectory

  handle <- openFile (home ++ "/haskell-backup.hsd") ReadMode
  contents <- hGetContents handle
  return ((read contents)::[BackupConfigOption])

  (r, w) <- createPipe
  fdWrite w "HITHERE\n"
  closeFd w




  home <- getHomeDirectory
  shell (
    intercalate " "
      [
        "tar", "-c", Data.Text.pack (home ++ "/Files/books"),
        "|",
        "gpg",
        "--passphrase-fd", Data.Text.pack (show r),
        "--symmetric", "--batch",
        "--output", "encrypted.gpg"
      ]
    ) Turtle.empty
  return ()

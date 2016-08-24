{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Turtle
import Data.Text
import System.Directory
import System.Posix.IO


main :: IO ()
main = do
  home <- getHomeDirectory
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

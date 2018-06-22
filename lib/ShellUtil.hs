{-# LANGUAGE OverloadedStrings #-}

module ShellUtil where

import Prelude hiding (FilePath, concat)
import Turtle
import Data.Text

expandGlob :: Text -> Shell Line
expandGlob glob =
  inshell (concat ["for f in ", glob, "; do echo $f; done"] ) Turtle.empty

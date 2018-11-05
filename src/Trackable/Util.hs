{-# LANGUAGE OverloadedStrings #-}

module Trackable.Util where

import qualified Turtle as Tu
import qualified Data.Text as T
import Data.Monoid ((<>))

pathToTextOrError ::  Tu.FilePath -> T.Text
pathToTextOrError path =
  case (Tu.toText path) of
    Left l ->
      "(Error decoding path; approximation: " <> l <> ")"
    Right r -> r

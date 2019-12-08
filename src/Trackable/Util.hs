{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Trackable.Util where

import qualified Turtle as Tu
import qualified Data.Text as T
import Control.Exception.Base
import Control.Monad.Catch


data NoStringConversion = NoStringConversion T.Text
  deriving (Show)

instance Exception NoStringConversion

pathToTextOrError ::  Tu.FilePath -> Tu.Shell T.Text
pathToTextOrError path =
  case (Tu.toText path) of
    Left l ->
      throwM $ NoStringConversion l
    Right r -> pure r

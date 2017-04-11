{-# LANGUAGE OverloadedStrings #-}

module Foo where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Control.Applicative
import Prelude -- Ensure Applicative is in scope and we have no warnings, before/after AMP.

data Config =
  Config {
    haveFun :: Text
  , gitDirectories :: [Text]
  } deriving (Eq, Show)
(Y.decodeEither ("\nhave-fun: scooty dooty\ngit-directories:\n - foo") ) :: Either String Config
instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .:   "have-fun" <*>
    v .:   "git-directories"

  parseJSON _ = fail "poopy"

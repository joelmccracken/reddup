{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Turtle
import Data.Text
-- import System.Directory


main :: IO ()
main = do
  cd "~/Projects/git-stuff"


-- main :: IO ()
-- main = do
--   (status, output) <- shellStrict "brew list --versions" Turtle.empty
--   putStrLn $ show $ Prelude.map (splitOn " ") $ splitOn "\n" output

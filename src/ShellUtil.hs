{-# LANGUAGE OverloadedStrings #-}

module ShellUtil where

import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import Data.Text
import qualified System.Process as SP

expandGlob :: Text -> Tu.Shell Tu.Line
expandGlob glob =
  Tu.inshell (concat ["for f in ", glob, "; do echo $f; done"] ) Tu.empty

type EnvVars = [(String, String)]

mergeWithExistingEnv :: EnvVars -> IO EnvVars
mergeWithExistingEnv adtlVars = do
  let unpack' kv = (unpack $ fst kv, unpack $ snd kv)
  e <- Tu.env
  return (adtlVars ++ (unpack' <$> e))

openInteractiveShell :: EnvVars -> IO ()
openInteractiveShell adtlVars = do
  let handler _ _ _ p = SP.waitForProcess p
  envVars <- mergeWithExistingEnv adtlVars
  let cmd = (SP.shell "bash") {
        SP.delegate_ctlc = True,
        SP.env = Just envVars }
  _ <- SP.withCreateProcess cmd handler
  return ()

shellCmdWithEnv :: Text -> EnvVars -> IO ()
shellCmdWithEnv cmd adtlVars = do
  let handler _ _ _ p = SP.waitForProcess p
  envVars <- mergeWithExistingEnv adtlVars
  let cmd' = (SP.shell (unpack cmd)) {
        SP.delegate_ctlc = True,
        SP.env = Just envVars }
  _ <- SP.withCreateProcess cmd' handler
  return ()

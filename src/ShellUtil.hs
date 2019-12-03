{-# LANGUAGE OverloadedStrings #-}

module ShellUtil where

import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import Data.Text
import qualified System.Process as SP

expandGlob :: Text -> Tu.Shell Tu.Line
expandGlob glob =
  Tu.inshell ("for f in " <> glob <> "; do echo $f; done") Tu.empty


-- returns the first item in a shell
-- seems like there should be a better way
firstShell :: Tu.Shell a -> Tu.Shell (Maybe a)
firstShell shell =
  let
    useFirst = Tu.Fold (Tu.<|>) Nothing id
  in
    Tu.fold (Just <$> shell) useFirst

expandOne :: Text -> Tu.Shell (Maybe Tu.Line)
expandOne glob =
  firstShell $ expandGlob glob

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

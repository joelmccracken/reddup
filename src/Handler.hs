{-# LANGUAGE OverloadedStrings #-}

module Handler where

import qualified GitParse as GP
import qualified Data.Text as T
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
-- import qualified System.IO as SIO
import Data.Monoid ((<>))
import Trackable.Data
import Trackable.Util
import qualified System.IO as IO
import qualified ShellUtil
import qualified Config as C
import qualified Data.Map.Strict as M
import qualified Reddup  as R
import Control.Monad.Reader (ask, lift, runReaderT, liftIO)
import qualified Data.List as List

inboxPrintHandler :: NHFile -> Tu.Shell ()
inboxPrintHandler (NHFile (InboxDirTrackable inbox _locSpec) file) =
  Tu.liftIO $ putStrLn $ T.unpack $ formatted
  where
    formatted =
      (pathToTextOrError inbox) <> ": file present " <> (pathToTextOrError file)

inboxInteractiveHandler :: NHFile -> R.Reddup ()
inboxInteractiveHandler nh = do
  inboxHandler' nh

inboxHandler' :: NHFile -> R.Reddup ()
inboxHandler' nh@(NHFile (InboxDirTrackable inbox locSpec) file) = do
  let isIgnored = isFileIgnored file locSpec
  if isIgnored then
    R.verbose $ (T.pack $ Tu.encodeString file) <> " is in ignored list. skipping."
  else do
    R.verbose $ (T.pack $ Tu.encodeString file) <> " is not in ignored list, handling."
    inboxHandler'' nh

inboxHandler'' :: NHFile -> R.Reddup ()
inboxHandler'' nh@(NHFile (InboxDirTrackable inbox locSpec) file) = do
  reddup <- ask
  let config = R.reddupConfig reddup
  let run call = runReaderT call reddup
  let
    fmtMsg =
      (pathToTextOrError inbox) <>
      ": file present " <>
      (pathToTextOrError file)
    inboxHandlerCommands = C.inboxHandlerCommands config
    envVars = [("FILE", Tu.encodeString file)]
  Tu.liftIO $ do
    putStrLn $ T.unpack $ fmtMsg
    putStrLn "Action choices:"
    putStrLn "(d)elete"
    putStrLn "(r)ename file"
    putStrLn "open a (s)hell"
    printMenuCustomCommands $ M.elems inboxHandlerCommands
    putStrLn "(q)uit"
    putStr "Selection: "
    IO.hFlush IO.stdout
    selection <- getLine
    case selection of
      "d" -> do
        putStrLn "deleting."
        Tu.sh $ do
          Tu.rm file
      "s" -> do
        putStrLn "Starting bash. Reddup will continue when subshell exits."
        putStrLn "Filename available in shell as $FILE."
        ShellUtil.openInteractiveShell envVars
        Tu.sh $ do
          destinationExists <- Tu.testfile file
          if destinationExists then do
            Tu.liftIO $ putStrLn "file still exists, continuing processing"
            run (inboxHandler' nh)
          else
            Tu.liftIO $ putStrLn "file no longer exists, continuing to next file"
      "n" ->
        putStrLn "going to next."
        -- just return from this handler, nothing left to do
      "r" ->
        Tu.sh $ run $ handleRename nh
      "q" -> do
        Tu.sh $ Tu.exit Tu.ExitSuccess
      _ -> do
        putStrLn $ show $ inboxHandlerCommands
        let result = M.lookup (T.pack $ selection) inboxHandlerCommands
        case result of
          Just cmd -> Tu.sh $ do
            _ <- Tu.liftIO $ ShellUtil.shellCmdWithEnv (C.cmdSpecCmd cmd) envVars
            run $ inboxHandler' nh
          Nothing -> do
            putStrLn $ "input unrecognized: '" <> selection <>"'"
            Tu.sh $ run $ inboxHandler' nh

isFileIgnored :: FilePath -> C.LocationSpec -> Bool
isFileIgnored file locSpec =
  let
    ignored = ignoredFiles locSpec
    dir = Tu.parent file
    ignored' = (dir Tu.</>) <$> ignored
  in
    List.any (file ==) ignored'

ignoredFiles :: C.LocationSpec -> [Tu.FilePath]
ignoredFiles locSpec =
  case locSpec of
    C.GitLoc _loc -> []
    C.InboxLoc _loc ignoredFiles ->
      maybe [] ((Tu.fromString . T.unpack) <$>) ignoredFiles

printMenuCustomCommands :: [C.InboxHandlerCommandSpec] -> IO ()
printMenuCustomCommands ihcSpecs = do
  foldr (>>) (return ()) ((putStrLn . T.unpack . C.cmdName) <$> ihcSpecs)

handleRename :: NHFile -> R.Reddup ()
handleRename nh@(NHFile _inbox filePath) = do
  reddup <- ask
  let run cmd = runReaderT cmd reddup
  lift $ Tu.liftIO $ do
    putStrLn $ "Renaming file. original name " <> (T.unpack $ pathToTextOrError filePath)
    putStr $ "Enter new name: "
    IO.hFlush IO.stdout
    newName <- getLine
    let newPath = (Tu.directory filePath) Tu.</> (Tu.fromText $ T.pack newName)
    destinationExists <- Tu.testfile newPath
    if destinationExists then do
      putStrLn "Error, destination exists. Choose another name."
      Tu.sh $ run $ handleRename nh
    else do
      putStrLn $ "new name: " <> newName
      putStrLn "(a)ccept new name"
      putStrLn "(c)ancel renaming (go back to previous menu)"
      putStrLn "(t)ry again (enter a new name)"
      IO.hFlush IO.stdout
      renameSelection <- getLine
      case renameSelection of
        "a" -> do
          Tu.sh $ Tu.mv filePath newPath
        "c" ->
          Tu.sh $ run $ inboxHandler' nh
        "t" ->
          Tu.sh $ run $ handleRename nh
        _ -> do
          putStrLn $ "input unrecognized: '" <> renameSelection <>"'"
          Tu.sh $ run $ handleRename nh


gitPrintHandler :: NHGit -> Tu.Shell ()
gitPrintHandler (NHGit (GitRepoTrackable dir' _locSpec) nhg) =
  Tu.liftIO $ putStrLn $ T.unpack $ format
  where
    format =
      let dir = pathToTextOrError dir' in
      case nhg of
        NHStatus (GP.Added f) -> formatPath dir f "file added"
        NHStatus (GP.AddedAndModified f) -> formatPath dir f "file added and modified"
        NHStatus (GP.Staged f) -> formatPath dir f "staged changes"
        NHStatus (GP.Unstaged f) -> formatPath dir f "unstaged changes"
        NHStatus (GP.StagedAndUnstaged f) -> formatPath dir f "staged and unstaged changes"
        NHStatus (GP.Untracked f) -> formatPath dir f "untracked file"
        NHStatus (GP.Deleted f) -> formatPath dir f "file deleted"
        NHStatus (GP.Unknown f) -> formatPath dir f "(unknown git status)"
        NHUnpushedBranch (GP.GitBranch branchName) ->
          dir <> ": Unpushed branch '" <> branchName <> "'"
        NHNotGitRepo -> dir <> ": is not a git repo"
    formatPath :: T.Text -> T.Text -> T.Text -> T.Text
    formatPath path statusItem label =
      path <> ": " <> label <> " '" <> statusItem  <> "'"

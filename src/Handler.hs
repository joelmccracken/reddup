{-# LANGUAGE OverloadedStrings #-}

module Handler where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Prelude hiding (FilePath, concat)
import qualified Turtle as Tu
import Turtle ((</>))
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
import Data.Foldable (traverse_)

handleInbox ::  NHFile -> R.Reddup ()
handleInbox nh = do
  isInteractive <- R.isInteractive
  if isInteractive then
    inboxInteractiveHandler nh
  else
    inboxPrintHandler nh

inboxPrintHandler :: NHFile -> R.Reddup ()
inboxPrintHandler (NHFile (InboxDirTrackable inbox locSpec) file) = do
  let isIgnored = isFileIgnored file locSpec
  inbox' <- lift $ pathToTextOrError inbox
  file' <- lift $ pathToTextOrError file
  let formatted = inbox' <> ": file present " <> file'
  if isIgnored then do
    return ()
  else
    liftIO $ TIO.putStrLn $ formatted

inboxInteractiveHandler :: NHFile -> R.Reddup ()
inboxInteractiveHandler nh@(NHFile (InboxDirTrackable _inbox locSpec) file) = do
  file' <- lift $ pathToTextOrError file
  let isIgnored = isFileIgnored file locSpec
  if isIgnored then
    R.verbose $ file' <> " is in ignored list. skipping."
  else do
    R.verbose $ file' <> " is not in ignored list, handling."
    inboxHandlerMenu nh

inboxHandlerMenu :: NHFile -> R.Reddup ()
inboxHandlerMenu nh@(NHFile (InboxDirTrackable inbox _locSpec) file) = do
  reddup <- ask
  let config = R.reddupConfig reddup
  let run = (flip runReaderT) reddup
  inbox' <- lift $ pathToTextOrError inbox
  file' <- lift $ pathToTextOrError file
  let fmtMsg = inbox' <> ": file present " <> file'
  let inboxHandlerCommands = C.inboxHandlerCommands config
  Tu.echo $ Tu.fromString $ Tu.encodeString file
  let envVars = [("FILE", Tu.encodeString file)]
  selection <- Tu.liftIO $ do
    putStrLn $ T.unpack $ fmtMsg
    putStrLn "Action choices:"
    putStrLn "(d)elete"
    putStrLn "(r)ename file"
    putStrLn "open a (s)hell"
    putStrLn "continue to (n)ext"
    putStrLn "re(f)le to location"
    printMenuCustomCommands $ M.elems inboxHandlerCommands
    putStrLn "(q)uit"
    putStr "Selection: "
    IO.hFlush IO.stdout
    getLine
  case selection of
    "d" -> do
      Tu.echo "deleting."
      Tu.rm file
    "s" -> do
      Tu.echo "Starting bash. Reddup will continue when subshell exits."
      Tu.echo "Filename available in shell as $FILE."
      Tu.liftIO $ ShellUtil.openInteractiveShell envVars
      Tu.sh $ do
        destinationExists <- Tu.testfile file
        if destinationExists then do
          Tu.echo "file still exists, continuing processing"
          run (inboxInteractiveHandler nh)
        else
          Tu.echo "file no longer exists, continuing to next file"
    "f" -> do
      handleRefile nh
    "n" ->
      Tu.echo "continuing to to next item."
      -- just return from this handler, nothing left to do
    "r" ->
      handleRename nh
    "q" -> do
      Tu.exit Tu.ExitSuccess
    _ -> do
      R.debug $ T.pack $ show $ inboxHandlerCommands
      let result = M.lookup (T.pack $ selection) inboxHandlerCommands
      case result of
        Just cmd -> Tu.sh $ do
          _ <- Tu.liftIO $ ShellUtil.shellCmdWithEnv (C.cmdSpecCmd cmd) envVars
          run $ inboxInteractiveHandler nh
        Nothing -> do
          Tu.echo $ Tu.fromString $ "input unrecognized: '" <> selection <>"'"
          Tu.sh $ run $ inboxInteractiveHandler nh

isFileIgnored :: FilePath -> C.LocationSpec -> Bool
isFileIgnored file locSpec =
  let
    ignored = ignoredFiles locSpec
    dir = Tu.parent file
    ignored' = (dir </>) <$> ignored
  in
    List.any (file ==) ignored'

ignoredFiles :: C.LocationSpec -> [Tu.FilePath]
ignoredFiles locSpec =
  case locSpec of
    C.GitLoc _loc -> []
    C.InboxLoc _loc ignoredFiles' ->
      maybe [] ((Tu.fromString . T.unpack) <$>) ignoredFiles'

printMenuCustomCommands :: [C.InboxHandlerCommandSpec] -> IO ()
printMenuCustomCommands ihcSpecs = do
  printStrings $ (T.unpack . C.cmdName) <$> ihcSpecs

printRefileDests :: [C.InboxHandlerRefileDestSpec] -> IO ()
printRefileDests refileDests = do
  printStrings $ ( ("destination: " <>) . T.unpack . C.refileDestName) <$> refileDests

printStrings :: [String] -> IO ()
printStrings = traverse_ putStrLn

handleRefile :: NHFile -> R.Reddup ()
handleRefile nh@(NHFile _ filePath) = do
  reddup <- ask
  let config = R.reddupConfig reddup
  liftIO $ putStrLn $ show config
  let inboxRefileDests' = C.inboxRefileDests config
  filePath' <- lift $ pathToTextOrError filePath
  liftIO $ putStrLn $ "Refiling " <> (T.unpack filePath' )
  liftIO $ putStrLn $ "Choose destination, or (q) to quit: "
  liftIO $ printRefileDests $ M.elems inboxRefileDests'

  dest <- T.pack <$> (liftIO $ getLine)

  case dest of
    "q" -> do
      liftIO $ putStrLn "quitting."
    _ -> do
      let result = M.lookup dest inboxRefileDests'
      case result of
        Just target -> do
          R.debug $ T.pack $ show target
          refileTo nh target
        Nothing -> do
          liftIO $ putStrLn $ "destination unrecognized: '" <> T.unpack dest <>"'"
          handleRefile nh

refileTo :: NHFile -> C.InboxHandlerRefileDestSpec -> R.Reddup ()
refileTo nh@(NHFile _inboxTrackable filePath) refileDest = do
  let filename = Tu.filename filePath
  let destDirRaw = C.refileDestDir refileDest
  let accessError = liftIO $ putStrLn $ "problem accessing directoy " <> T.unpack destDirRaw
  mdestDir <- lift $ ShellUtil.expandOne destDirRaw
  case mdestDir of
    Just destDir -> do
      let
        destDirFP :: Tu.FilePath
        destDirFP = Tu.fromString $ T.unpack $ Tu.lineToText $ destDir
        newFilename = destDirFP </> filename
      testResult <- liftIO $ Tu.testdir destDirFP
      if testResult then do
        R.debug $ "Moving to dest " <> (T.pack $ Tu.encodeString newFilename)
        destExists <- Tu.testfile newFilename
        if destExists then do
          liftIO $ putStrLn "File currently exists at destination."
          handleRefile nh
        else do
          Tu.mv filePath newFilename
      else do
        accessError
    Nothing -> do
      accessError

handleRename :: NHFile -> R.Reddup ()
handleRename nh@(NHFile inbox filePath) = do
  reddup <- ask
  let run cmd = runReaderT cmd reddup
  filePath' <- lift $ pathToTextOrError filePath
  lift $ Tu.liftIO $ do
    putStrLn $ "Renaming file. original name " <> (T.unpack filePath')
    putStr $ "Enter new name: "
    IO.hFlush IO.stdout
    newName <- getLine
    let newPath = (Tu.directory filePath) </> (Tu.fromText $ T.pack newName)
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
          Tu.sh $ do
            Tu.mv filePath newPath
            let nh' = NHFile inbox newPath
            run $ inboxInteractiveHandler nh'
        "c" ->
          Tu.sh $ run $ inboxInteractiveHandler nh
        "t" ->
          Tu.sh $ run $ handleRename nh
        _ -> do
          putStrLn $ "input unrecognized: '" <> renameSelection <>"'"
          Tu.sh $ run $ handleRename nh

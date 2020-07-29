{-# LANGUAGE OverloadedStrings #-}

module Handler.Git where

import Control.Monad (forever)
import qualified Data.Text as T
import Trackable.Data
import Trackable.Util
import qualified GitParse as GP
import qualified Turtle as Tu
import qualified Handler as H
import qualified Reddup  as R
import Control.Monad.Reader (ask, lift, runReaderT, liftIO, guard)
import qualified Git
import qualified Control.Foldl as L
import qualified System.IO as IO
import qualified ShellUtil

gitHandler' :: GitRepoTrackable -> R.Reddup ()
gitHandler' grt@(GitRepoTrackable dir locSpec) = do
  R.verbose $ "checking " <> (T.pack $ show dir)
  lift $ Tu.cd dir
  dirExists <- lift $ Tu.testdir ".git"
  if dirExists then do
    isInteractive <- R.isInteractive
    if isInteractive then
      processGitInteractive grt
    else
         if H.force locSpec
            then do
                lift $ processGitNonInteractiveForce grt
            else lift $ processGitNonInteractive grt
  else
    errorNotGitRepo grt

numShell :: Tu.Shell a -> Tu.Shell Int
numShell sh = Tu.fold sh L.length

processGitInteractive :: GitRepoTrackable -> R.Reddup ()
processGitInteractive grt@(GitRepoTrackable dir _locSpec) = do
  R.verbose $ T.pack $ "Git Repo: " <> Tu.encodeString dir
  let gitStatus = checkGitStatus grt

  numStatus <- lift $ numShell gitStatus

  if numStatus > 0 then
    processGitWorkDir grt
  else
    return ()

  let gitUnpushed = Git.unpushedGitBranches

  numUnpushed <- lift $ numShell Git.unpushedGitBranches

  if numUnpushed > 0 then
    processGitUnpushed grt gitUnpushed
  else
    return ()

processGitWorkDir :: GitRepoTrackable -> R.Reddup ()
processGitWorkDir grt@(GitRepoTrackable dir _locSpec) = do
  reddup <- ask
  let
    run :: R.Reddup () -> IO ()
    run = Tu.sh . (flip runReaderT) reddup

  liftIO $ putStrLn $
    "Git repo " <>
    Tu.encodeString dir <>
    ": workdir dirty "

  liftIO $ do
    putStrLn "Actions:"
    putStrLn "open a (s)hell"
    putStrLn "git (d)iff (diff of working dir and index)"
    putStrLn "git d(i)ff --cached (diff of index and HEAD)"
    putStrLn "git s(t)atus"
    putStrLn "(w)ip commit (`git add .; git commit -m 'WIP'` )"
    putStrLn "continue to (n)ext item"
    putStrLn "(q)uit"
    putStr "Choice: "
    IO.hFlush IO.stdout
    selection <- getLine
    case selection of
      "s" -> do
        putStrLn "Starting bash. Reddup will continue when subshell exits."
        ShellUtil.openInteractiveShell []
        run $ processGitInteractive grt
      "d" -> do
        Tu.sh $ Tu.shell "git diff" Tu.empty
        run $ processGitInteractive grt
      "n" ->
        putStrLn "continuing to next."
      "i" -> do
        Tu.sh $ Tu.shell "git diff --cached" Tu.empty
        run $ processGitInteractive grt
      "t" -> do
        Tu.sh $ Tu.shell "git status" Tu.empty
        run $ processGitInteractive grt
      "w" -> do
        Tu.sh $ Tu.inshell "git add .; git commit -m 'WIP'" Tu.empty
        run $ processGitInteractive grt
      "q" -> do
        Tu.sh $ Tu.exit Tu.ExitSuccess
      _ -> do
        putStrLn $ "input unrecognized: '" <> selection <>"'"
        run $ processGitInteractive grt

processGitUnpushed :: GitRepoTrackable -> Tu.Shell GP.GitBranch -> R.Reddup ()
processGitUnpushed grt@(GitRepoTrackable dir _locSpec) branchShell = do
  reddup <- ask

  let
    run :: R.Reddup () -> IO ()
    run = Tu.sh . (flip runReaderT) reddup

  branch@(GP.GitBranch branchName) <- lift $ branchShell

  bl <- Tu.fold Git.unpushedGitBranches L.list

  guard $ elem branch bl

  liftIO $ putStrLn $
    "Git repo " <>
    Tu.encodeString dir <>
    ": unpushed branch "  <>
    T.unpack branchName

  target <- lift $ readPushTarget branch
  merge <- lift $ readMerge branch

  let targetAndMerge = (,) <$> target <*> merge

  let unrecognized selection = do
        putStrLn $ "input unrecognized: '" <> selection <>"'"
        run $ processGitInteractive grt

  liftIO $ do
    putStrLn "target"
    putStrLn $ show target
    putStrLn "Actions:"
    putStrLn "open a (s)hell"
    gitPushLabel branchName targetAndMerge
    putStrLn "continue to (n)ext item"
    putStrLn "(q)uit"
    putStr "Choice: "
    IO.hFlush IO.stdout
    selection <- getLine
    case selection of
      "s" -> do
        putStrLn "Starting bash. Reddup will continue when subshell exits."
        ShellUtil.openInteractiveShell []
        run $ processGitInteractive grt
      "n" ->
        putStrLn "continuing to next."
      "p" -> do
        gitPushCmd branchName targetAndMerge (unrecognized selection)
      "q" -> do
        Tu.sh $ Tu.exit Tu.ExitSuccess
      _ -> do
        unrecognized selection

readPushTarget :: GP.GitBranch -> Tu.Shell (Maybe T.Text)
readPushTarget branch = do
  pr <- readPushRemote branch
  r  <- readRemote branch
  return $ pr Tu.<|> r

readGitConfig :: T.Text -> Tu.Shell (Maybe T.Text)
readGitConfig cmd = do
  mConfigOutput <- ShellUtil.firstShell $ Tu.inshell cmd Tu.empty
  return $ Tu.lineToText <$> mConfigOutput

readRemote :: GP.GitBranch -> Tu.Shell (Maybe T.Text)
readRemote (GP.GitBranch branchName) = do
  let command = "git config branch." <> branchName <> ".remote || true"
  readGitConfig command

readPushRemote :: GP.GitBranch -> Tu.Shell (Maybe T.Text)
readPushRemote (GP.GitBranch branchName) = do
  let command = "git config branch." <> branchName <> ".pushRemote || true"
  readGitConfig command

readMerge :: GP.GitBranch -> Tu.Shell (Maybe T.Text)
readMerge (GP.GitBranch branchName) = do
  let command = "git config branch." <> branchName <> ".merge || true"
  readGitConfig command

gitPushLabel :: T.Text -> Maybe (T.Text, T.Text) -> IO ()
gitPushLabel branchName =
  maybe unavailable (uncurry available)
  where
    unavailable =
      putStrLn "(git push not available, pushremote and/or merge configuration not set)"
    available target merge =
      putStrLn $ "git (p)ush " <> T.unpack target <> " " <> T.unpack branchName <> ":" <> T.unpack merge

gitPushCmd :: T.Text -> Maybe (T.Text, T.Text) -> IO () -> IO ()
gitPushCmd branchName targetAndMerge unrecognized =
  case targetAndMerge of
    Just (target, merge) ->
      let
        cmd = "git push -v " <> target <> " " <> branchName <> ":" <> merge
      in
        Tu.sh $ Tu.shell cmd Tu.empty
    Nothing -> unrecognized

checkGitProblems :: GitRepoTrackable -> Tu.Shell NHGit
checkGitProblems grt =
  checkGitUnpushed grt Tu.<|> checkGitStatus grt

checkGitStatus :: GitRepoTrackable -> Tu.Shell NHGit
checkGitStatus grt =
  NHGit grt . NHStatus <$> Git.gitStatus

checkGitUnpushed :: GitRepoTrackable -> Tu.Shell NHGit
checkGitUnpushed grt =
  NHGit grt . NHUnpushedBranch <$> Git.unpushedGitBranches

gitPrintHandler :: NHGit -> Tu.Shell ()
gitPrintHandler (NHGit (GitRepoTrackable dir' _locSpec) nhg) = do
  dir <- pathToTextOrError dir'
  let formatPath :: T.Text -> T.Text -> T.Text -> T.Text
      formatPath path statusItem label =
        path <> ": " <> label <> " '" <> statusItem  <> "'"
      format =
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
  liftIO $ putStrLn $ T.unpack $ format

processGitNonInteractive :: GitRepoTrackable -> Tu.Shell ()
processGitNonInteractive grTrack = do
  checkGitProblems grTrack >>= gitPrintHandler

processGitNonInteractiveForce :: GitRepoTrackable -> Tu.Shell ()
processGitNonInteractiveForce grTrack =
    forever $
        checkGitProblems grTrack >>= tryFixGitProblem

tryFixGitProblem :: NHGit -> Tu.Shell ()
tryFixGitProblem nh@(NHGit (GitRepoTrackable _dir _locSpec) nhg) = do
    case nhg of
        NHStatus (GP.Added _) ->
            addAndWipCommit
        NHStatus (GP.AddedAndModified _) ->
            addAndWipCommit
        NHStatus (GP.Staged _) ->
            addAndWipCommit
        NHStatus (GP.Unstaged _) ->
            addAndWipCommit
        NHStatus (GP.StagedAndUnstaged _) ->
            addAndWipCommit
        NHStatus (GP.Untracked _) ->
            addAndWipCommit
        NHStatus (GP.Deleted _) ->
            addAndWipCommit
        NHUnpushedBranch branch -> do
            targetAndMerge <- (,) <$> readPushTarget branch <*> readMerge branch
            let (GP.GitBranch branchName) = branch
            liftIO $ gitPushCmd branchName (go targetAndMerge) mempty
            pure ()
        _ -> do
            gitPrintHandler nh
            Tu.mzero
  where
    go :: (Maybe a, Maybe a) -> Maybe (a, a)
    go (Just a, Just b) = Just (a, b)
    go _ = Nothing

addAndWipCommit :: Tu.MonadIO m => m ()
addAndWipCommit =
    Tu.sh $ Tu.inshell "git add .; git commit -m 'reddup auto WIP commit'" Tu.empty

errorNotGitRepo :: GitRepoTrackable -> R.Reddup ()
errorNotGitRepo (GitRepoTrackable dir _locSpec) = do
  liftIO $ putStrLn $ "Warning: Not a git repository: " <> Tu.encodeString dir

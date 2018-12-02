{-# LANGUAGE OverloadedStrings #-}

module Handler.Git where

import qualified Data.Text as T
import Trackable.Data
import Trackable.Util
import qualified GitParse as GP
import qualified Turtle as Tu
import qualified Reddup  as R
import Control.Monad.Reader (ask, lift, runReaderT, liftIO, guard)
import qualified Git
import qualified Control.Foldl as L
import qualified System.IO as IO
import qualified ShellUtil

gitHandler' :: GitRepoTrackable -> R.Reddup ()
gitHandler' grt@(GitRepoTrackable dir _locSpec) = do
  R.verbose $ "checking " <> (T.pack $ show dir)
  lift $ Tu.cd dir
  dirExists <- lift $ Tu.testdir ".git"
  if dirExists then do
    isInteractive <- R.isInteractive
    if isInteractive then
      processGitInteractive grt
    else
      processGitNonInteractive grt
  else
    errorNotGitRepo grt

numShell :: Shell a -> boolean
numShell sh = Tu.fold sh L.length


processGitInteractive :: GitRepoTrackable -> R.Reddup ()
processGitInteractive grt@(GitRepoTrackable dir _locSpec) = do
  R.verbose $ T.pack $ "Git Repo: " <> Tu.encodeString dir
  let gitStatus = checkGitStatus grt
  let gitUnpushed = checkGitUnpushed grt

  numStatus <- lift $ numShell gitStatus
  numUnpushed <- lift $ numShell gitUnpushed



  numStatus <- lift $ Tu.fold (checkGitStatus grt) L.length
  R.verbose $ T.pack $ Tu.encodeString dir <> " issues " <> show numIssues
  -- go to next if no problems with this repo
  guard $ numIssues > 0


  let gitStatus = checkGitStatus grt
  numIssues <- lift $ Tu.fold (checkGitProblems grt) L.length


  processGitInteractive' grt

processGitInteractive' :: GitRepoTrackable -> R.Reddup ()
processGitInteractive' grt@(GitRepoTrackable dir _locSpec) = do
  R.verbose $ T.pack $ "Git Repo: " <> Tu.encodeString dir
  numIssues <- lift $ Tu.fold (checkGitProblems grt) L.length
  R.verbose $ T.pack $ Tu.encodeString dir <> " issues " <> show numIssues
  -- go to next if no problems with this repo
  guard $ numIssues > 0

  liftIO $ putStrLn $ "Git Repo: " <> Tu.encodeString dir <> " issues " <> show numIssues

  reddup <- ask

  let
    run :: R.Reddup () -> IO ()
    run = Tu.sh . (flip runReaderT) reddup

  -- TODO show summary of issues
  -- E.g. "working directory" or "index" or "working dir AND index" <> "has issues"
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
  -- lift $ Git.unpushedGitBranches >>= (liftIO . putStrLn . show)






checkGitProblems :: GitRepoTrackable -> Tu.Shell NHGit
checkGitProblems grt = do
  checkGitUnpushed grt Tu.<|> checkGitStatus grt

checkGitStatus :: GitRepoTrackable -> Tu.Shell NHGit
checkGitStatus grt =
  NHGit grt <$> NHStatus <$> Git.gitStatus

checkGitUnpushed :: GitRepoTrackable -> Tu.Shell NHGit
checkGitUnpushed grt =
  NHGit grt <$> NHUnpushedBranch <$> Git.unpushedGitBranches

gitPrintHandler :: NHGit -> R.Reddup ()
gitPrintHandler (NHGit (GitRepoTrackable dir' _locSpec) nhg) =
  liftIO $ putStrLn $ T.unpack $ format
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

processGitNonInteractive :: GitRepoTrackable -> R.Reddup  ()
processGitNonInteractive grTrack =
  (lift $ checkGitProblems grTrack) >>= gitPrintHandler

errorNotGitRepo :: GitRepoTrackable -> R.Reddup ()
errorNotGitRepo (GitRepoTrackable dir _locSpec) = do
  liftIO $ putStrLn $ "Warning: Not a git repository: " <> Tu.encodeString dir

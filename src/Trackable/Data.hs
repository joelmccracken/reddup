module Trackable.Data where

import qualified Turtle as Tu
import qualified GitParse as GP
import Prelude hiding (FilePath)
import qualified Config as C

type GitRepoPath = Tu.FilePath
type InboxPath = Tu.FilePath
type FilePath = Tu.FilePath

data Trackable
  = GitRepo GitRepoTrackable
  | InboxDir InboxDirTrackable
  deriving (Show, Eq)

data GitRepoTrackable
  = GitRepoTrackable GitRepoPath C.LocationSpec
  deriving (Show, Eq)

data InboxDirTrackable
  = InboxDirTrackable InboxPath C.LocationSpec
  deriving (Show, Eq)

data NHGit
  = NHGit GitRepoTrackable NHGitItem
  deriving (Show, Eq)

data NHFile
  = NHFile InboxDirTrackable FilePath
  deriving (Show, Eq)

data NHGitItem
  = NHStatus GP.GitStatus
  | NHUnpushedBranch GP.GitBranch
  | NHNotGitRepo
  deriving (Show, Eq)

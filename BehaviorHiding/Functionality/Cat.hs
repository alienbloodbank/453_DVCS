module BehaviorHiding.Functionality.Cat(performCat) where

import System.Directory (doesDirectoryExist, doesPathExist)

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.Repo

performCat :: String -> String -> IO String
performCat revid file = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
  else do
    let commit_path = commitPath (CommitID revid)
    isPath <- doesPathExist commit_path
    if not(isPath) then return "fatal: invalid commit id."
    else do
      cur_file <- getCommitFile (CommitID revid) file
      return cur_file

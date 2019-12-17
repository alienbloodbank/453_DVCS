module BehaviorHiding.Functionality.Heads(performHeads) where

import System.Directory (doesDirectoryExist)
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.Repo

performHeads :: IO String
performHeads = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
  else do
    commit_head <- getHEAD
    if commit_head == (CommitID "root") then return "fatal: no commits in current repository."
    else do
      putStrLn $ "Commit: " ++ (getStr commit_head)
      message <- getCommitMessage commit_head
      putStrLn $ "Message: " ++ message
      time <- getCommitDate commit_head
      putStrLn $ "Time: " ++ (show time) ++ "\n"
      return "Heads shown"

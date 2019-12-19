module BehaviorHiding.Functionality.Log(performLog) where

import System.Directory(doesDirectoryExist)
--
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.MetaOrganization

import Control.Monad.ListM
import Control.Monad

performLog :: IO String
performLog = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
  else do
    commit_head <- getHEAD
    if commit_head == (CommitID "root") then return "fatal: no commits in current repository."
    else do
      com_list <- (getUpToRootRecursive [commit_head] >>= sortByM (\x y -> compare <$> (getCommitDate y) <*> (getCommitDate x)))
      history <- init <$> (filterM (\f -> doesDirectoryExist (objectPath ++ "/" ++ (getStr f))) com_list)
      putStrLn "(HEAD)"
      mapM_ (\com -> do
                     commit_message <- getCommitMessage com
                     commit_date <- getCommitDate com
                     putStrLn $ "Commit: " ++ (getStr com) ++ "\n" ++
                                "Message: " ++ commit_message ++ "\n" ++
                                "Time: " ++ (show commit_date) ++ "\n") history
      return "Commit history"

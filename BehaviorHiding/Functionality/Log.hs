module BehaviorHiding.Functionality.Log(performLog) where

import System.Directory(doesDirectoryExist)
--
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.Repo

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
      history <- (reverse . tail) <$> getUpToHead
      putStrLn "(HEAD)"
      mapM_ (\com -> do
                     commit_message <- getCommitMessage com
                     commit_date <- getCommitDate com
                     putStrLn $ "Commit: " ++ (getStr com) ++ "\n" ++
                                "Message: " ++ commit_message ++ "\n" ++
                                "Time: " ++ (show commit_date) ++ "\n") history
      return "Commit history"

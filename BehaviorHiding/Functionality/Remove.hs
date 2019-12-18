module BehaviorHiding.Functionality.Remove(performRemove) where

import System.Directory (doesDirectoryExist)

import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Concept.Repo

performRemove :: String -> IO String
performRemove file = do
   doesExist <- isRepo
   isLocked <- doesDirectoryExist ".LOCKED"
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else if isLocked then return "fatal: Please resolve conflicts and then commit them"
   else do
     trackedFiles <- getTrackedSet
     if (file `notElem` trackedFiles) then return "Error: File not being tracked. Nothing to remove"
     else TS.removeFile file >> return "File removed"

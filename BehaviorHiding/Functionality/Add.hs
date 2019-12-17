module BehaviorHiding.Functionality.Add(performAdd) where

import System.Directory (doesFileExist)

import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Concept.Repo

performAdd :: String -> IO String
performAdd file = do
   doesExist <- isRepo
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else do
     inCD <- doesFileExist file
     trackedFiles <- getTrackedSet
     if not(inCD) then do
       if (file `notElem` trackedFiles) then return "fatal: File does not exist in current directory"
       else do
         TS.removeFile file
         return "File removed as its not in current directory"
     else do
       if file `elem` trackedFiles then return "File already tracked"
       else do
         addFile file
         return "File added"

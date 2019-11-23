module BehaviorHiding.Functionality(performInit,
performClone, 
performAdd, 
performRemove, 
performStatus, 
performHeads, 
performDiff, 
performLog, 
performCheckout, 
performCommit,
performCat,
performPull,
performPush) where

import System.Directory (doesDirectoryExist, createDirectory, getCurrentDirectory, copyFile, doesFileExist, doesPathExist, listDirectory)
import System.Environment
import System.Process
import Data.List

import SoftwareDecision.Concept.TrackedSet (addFile, removeFile, getTracketSet)

-- HELPER FUNCTION 1 --
areFilesValid :: [String] -> IO Bool
areFilesValid files = do
   if files == [] then return True
   else do
      let (file:res) = files
      doesExist <- doesFileExist file
      if not(doesExist) then return False
      else do
        areValid <- areFilesValid res
        return areValid

performInit :: IO String
performInit = do
   doesExist <- doesDirectoryExist "./.dvcs"
   cd <- getCurrentDirectory
   if doesExist then return ("Reinitialized existing dvcs repository in " ++ cd)
   else do
      createDirectory "./.dvcs"
      -- create root commit
      writeFile "./.dvcs/repometadata.json" "{\"pid\":\"<pid>\",\"ts\":[], \"head_\":\"root\"}"
      writeFile "./.dvcs/repometadatatemp.json" "" 
      return "Initialized repo"

------------------------------------
performClone :: String -> IO String
performClone repo_path = do
   isLocalPath <- doesPathExist repo_path
   if isLocalPath then do
     _ <- readProcess "cp" ["-r", repo_path, "."] ""
     return "Cloned local repo"
   else do
     _ <- readProcess "scp" ["-r", repo_path, "."] ""
     return "Cloned remote repo"

------------------------------------
-- HELPER FUNCTION 2 --
addfiles :: [String] -> IO ()
addfiles files = do
   if files == [] then return ()
   else do
     let (file:res) = files
     addFile file
     copyFile "./.dvcs/repometadatatemp.json" "./.dvcs/repometadata.json"
     addfiles res

performAdd :: [String] -> IO String
performAdd files = do
   doesExist <- doesDirectoryExist "./.dvcs"
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else do
     areValid <- areFilesValid files
     if not(areValid) then return "fatal: some files do not exist in CD"
     else do
       addfiles files
       return "Files added"

------------------------------------
-- HELPER FUNCTION 2 --
rmfiles :: [String] -> IO ()
rmfiles files = do
   if files == [] then return ()
   else do
     let (file:res) = files
     removeFile file
     copyFile "./.dvcs/repometadatatemp.json" "./.dvcs/repometadata.json"
     rmfiles res

performRemove :: [String] -> IO String
performRemove files = do
   doesExist <- doesDirectoryExist "./.dvcs"
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else do
     areValid <- areFilesValid files
     if not(areValid) then return "fatal: some files do not exist in CD"
     else do
       rmfiles files
       return "Files removed"

------------------------------------
performStatus :: IO String
performStatus = do 
   trackedFiles <- getTracketSet
   putStrLn "Tracked files:"
   Prelude.mapM_ putStrLn trackedFiles
   putStrLn "\nUntracked files:"   
   allFiles <- listDirectory "."
   Prelude.mapM_ putStrLn (allFiles \\ trackedFiles)
   return "\ndvcs status output"

-- TODO --
------------------------------------
performHeads :: IO String
performHeads = do return "dvcs heads output"

performDiff :: String -> String -> IO String
performDiff revid1 revid2 = do return "dvcs diff output"

performLog :: IO String
performLog = do return "dvcs log output"

performCheckout :: String -> IO String
performCheckout revid = do return "Checked out"

performCommit :: String -> IO String
performCommit msg = do return "Committed"

performCat :: String -> String -> IO String
performCat revid file = do return "dvcs cat output"

performPull :: String -> IO String
performPull repo_path = do return "Pulled"

performPush :: String -> IO String
performPush repo_path = return "Pushed"


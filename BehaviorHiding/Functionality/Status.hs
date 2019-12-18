module BehaviorHiding.Functionality.Status(performStatus) where

import System.Directory (doesFileExist)
import Data.List

import Control.Monad

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Utility.DvcsInterface
import BehaviorHiding.Functionality.Helpers

performStatus :: IO String
performStatus = do
   doesExist <- isRepo
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else do
   -- Not using cleanTrackedSet here as its the responsibility of commit
   trackedFiles <- getTrackedSet
   trackedExistingFiles <- filterM doesFileExist trackedFiles
   unless (trackedExistingFiles == []) $ do
                                           putStrLn "Tracked files"
                                           mapM_ putStrLn trackedExistingFiles
                                           putStrLn ""
   let deletedFiles = trackedFiles \\ trackedExistingFiles
   unless (deletedFiles == []) (do
                                   putStrLn "Tracked files that no longer exist!"
                                   mapM_ putStrLn deletedFiles
                                   putStrLn "")
   commit_head <- getHEAD
   unless (commit_head == (CommitID "root")) $ do
                                                 let com_path = commitPath commit_head
                                                 commitedFiles <- listDirectoryRecursive com_path
                                                 alteredFiles <- filterM (\f -> do
                                                                              if (f `notElem` commitedFiles) then return False
                                                                              else do (checkAltered commit_head f) >>= return) trackedExistingFiles
                                                 unless (alteredFiles == []) (do
                                                                                 putStrLn "Altered files from last commit"
                                                                                 mapM_ putStrLn alteredFiles
                                                                                 putStrLn "")
   allFiles <- listDirectoryRecursive "."
   let untrackedFiles = allFiles \\ trackedFiles
   case untrackedFiles of [] -> putStrLn "Nothing is untracked!\n"
                          _ -> do
                                  putStrLn "Untracked files"
                                  mapM_ putStrLn untrackedFiles
                                  putStrLn ""
   return "Repository status"

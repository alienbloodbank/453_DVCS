module BehaviorHiding.Functionality.Checkout(performCheckout) where

import System.Directory (withCurrentDirectory,
                         removeFile,
                         doesDirectoryExist,
                         getCurrentDirectory,
                         doesPathExist,
                         copyFile,
                         createDirectoryIfMissing)

import Data.List
import Data.List.Split

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Utility.DvcsInterface
import BehaviorHiding.Functionality.Helpers

performCheckout :: String -> IO String
performCheckout revid = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
  else do
    let commit_path = commitPath (CommitID revid)
    isPath <- doesPathExist commit_path
    if not(isPath) then return "fatal: invalid commit id."
    else do
      isUnStashed <- checkUnstashed
      if isUnStashed then return "error: Please commit your changes or revert them before you checkout"
      else do
        cwd <- getCurrentDirectory
        trackedFiles <- getTrackedSet
        mapM_ (\f -> do
                        System.Directory.removeFile f
                        TS.removeFile f) trackedFiles

        files_in_rev <- listDirectoryRecursive commit_path >>= return . filter (/= commitMetaName)

        withCurrentDirectory commit_path $ do
                                            mapM_ (\x -> do
                                                          createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" x))
                                                          copyFile (x) (cwd ++ "/" ++ x)) files_in_rev
        mapM_ (\x -> TS.addFile x) files_in_rev
        setHEAD (CommitID revid)
        return ("Head successfully changed to " ++ revid)

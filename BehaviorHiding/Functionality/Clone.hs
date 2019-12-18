module BehaviorHiding.Functionality.Clone(performClone) where

import System.Directory (withCurrentDirectory,
                         removeFile,
                         removeDirectoryRecursive,
                         doesDirectoryExist,
                         doesPathExist,
                         createDirectoryIfMissing)

import System.FilePath.Posix

import Control.Monad

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.MetaOrganization
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Communication

performClone :: String -> IO String
performClone repo_path_impure = do
   let repoPath = normalise $ dropTrailingPathSeparator repo_path_impure
   isLocalPath <- doesPathExist repoPath
   if isLocalPath then do
     doesRepoExist <- doesDirectoryExist $ repoPath ++ "/" ++ dvcsPath
     if doesRepoExist then do
        copyDir "." repoPath
        let repo_name = takeBaseName repoPath
        -- Removing files not in the tracked set because clone copies everything
        withCurrentDirectory repo_name $ do
                                          trackedSet <- getTrackedSet
                                          allFiles <- listDirectoryRecursive "."
                                          mapM_ (\f -> when (f `notElem` trackedSet) (System.Directory.removeFile f)) allFiles
        return "Cloned local repository"
     else return "Local directory is not a valid repository"
   else do
     let (_, _:repo) = break (==':') repoPath
     downloadRemoteDir repoPath
     let repo_name = takeBaseName repo
     doesRepoExist <- doesDirectoryExist $ repo_name ++ "/" ++ dvcsPath
     if doesRepoExist then do
        -- Removing files not in the tracked set because clone copies everything
        withCurrentDirectory repo_name $ do
                                          trackedSet <- getTrackedSet
                                          allFiles <- listDirectoryRecursive "."
                                          mapM_ (\f -> when (f `notElem` trackedSet) (System.Directory.removeFile f)) allFiles
        return "Cloned remote repository"
     else removeDirectoryRecursive repo_name >> return "Remote directory is not a valid repository"

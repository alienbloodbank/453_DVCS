module BehaviorHiding.Functionality.Push(performPush) where

import System.Directory (withCurrentDirectory,
                         removeDirectoryRecursive,
                         doesDirectoryExist,
                         doesPathExist,
                         createDirectoryIfMissing)

import System.FilePath.Posix

import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.MetaOrganization
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Communication
import BehaviorHiding.Functionality.Merge


performPush :: String -> IO String
performPush repo_path_impure = do
   let repo_path = normalise $ dropTrailingPathSeparator repo_path_impure
   doesExist <- isRepo
   isLocked <- doesDirectoryExist ".LOCKED"
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else if isLocked then return "fatal: Please resolve conflicts and then commit them"
   else do
   isLocalPath <- doesPathExist repo_path
   if isLocalPath then do
     doesRepoExist <- doesDirectoryExist $ repo_path ++ "/" ++ dvcsPath
     if doesRepoExist then do
        copyRepo (LocalPath repo_path)
        msg <- mergePush
        copyDir repo_path (remoteLoc ++ "/.")
        removeDirectoryRecursive remoteLoc
        return msg
     else return "Local directory is not a valid repository"
   else do
     let (_, _:repo) = break (==':') repo_path
     withCurrentDirectory tempPath (downloadRemoteDir repo_path)
     renameDir (tempPath ++ "/" ++ (takeBaseName repo)) remoteLoc
     doesRepoExist <- doesDirectoryExist $ remoteLoc ++ "/" ++ dvcsPath
     if doesRepoExist then do
        msg <- mergePush
        withCurrentDirectory remoteLoc (uploadRemoteDir repo_path)
        removeDirectoryRecursive remoteLoc
        return msg
     else do
        removeDirectoryRecursive remoteLoc
        return "Remote directory is not a valid repository"

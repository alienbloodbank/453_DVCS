module BehaviorHiding.Functionality.Diff(performDiff) where

import System.Directory (withCurrentDirectory,
                         doesDirectoryExist,
                         doesPathExist,
                         createDirectoryIfMissing)
import System.Process
import System.FilePath.Posix
import Data.List
import Data.Algorithm.Diff

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Utility.DvcsInterface

performDiff :: String -> String -> IO String
performDiff revid1 revid2 = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
    else if revid1 == revid2 then return "fatal: ID's are identical"
      else do
        let path1 = commitPath (CommitID revid1)
        let path2 = commitPath (CommitID revid2)
        isPath1 <- doesPathExist path1
        isPath2 <- doesPathExist path2
        if revid1 == "root" || not(isPath1) then return ("fatal: invalid commit id. " ++ revid1)
        else if revid2 == "root" || not(isPath2) then return ("fatal: invalid commit id. " ++ revid2)
          else do
            files1 <- listDirectoryRecursive path1 >>= return . filter (/= commitMetaName)
            files2 <- listDirectoryRecursive path2 >>= return . filter (/= commitMetaName)
            let dFiles = getDiff files1 files2
            mapM_ (\snap -> case snap of
                                     (First f) -> do
                                                    putStrLn f
                                                    putStrLn $ "File not existant in second commit " ++ (takeBaseName path2) ++ "\n"
                                     (Second f) -> do
                                                    putStrLn f
                                                    putStrLn $ "File not existant in first commit " ++ (takeBaseName path1) ++ "\n"
                                     (Both a b) -> do
                                                    putStrLn a
                                                    system $ "diff " ++ (path1 ++ "/" ++ a) ++ " " ++ (path2 ++ "/" ++ b)
                                                    putStrLn "") dFiles
            return "Diff shown"

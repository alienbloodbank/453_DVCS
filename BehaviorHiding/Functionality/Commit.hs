module BehaviorHiding.Functionality.Commit(performCommit) where

import System.Directory (withCurrentDirectory,
                         removeDirectoryRecursive,
                         doesDirectoryExist,
                         copyFile,
                         createDirectoryIfMissing)

import System.IO.Unsafe
import Data.List
import Data.List.Split

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Utility.DvcsInterface
import BehaviorHiding.Functionality.Helpers

performCommit :: String -> IO String
performCommit msg = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then do
    removeDirectoryRecursive ".LOCKED"
    cleanTrackedSet -- remove files from TS if not in CD
    trackedFiles <- getTrackedSet
    hid <- getHEAD
    let commit_path = commitPath hid
    mapM_ (\x -> createDirectoryIfMissing True (commit_path ++ "/" ++ (intercalate "/" (init (splitOn "/" x))))) trackedFiles
    mapM_ (\x -> copyFile (x) (commit_path ++ "/" ++ x)) trackedFiles
    return "Finished merge commit"
  else do
  cleanTrackedSet -- remove files from TS if not in CD
  trackedFiles <- getTrackedSet
  if trackedFiles == []
    then return "Nothing to commit: tracked set empty or tracked file(s) not in current directory."
    else do
      cleanTrackedSet -- remove files from TS if not in CD
      trackedFiles <- getTrackedSet

      head_cid <- getHEAD
      -- putStrLn(getStr head_cid) -- print out HEAD

      if (getStr head_cid) == "root"
        then do
          -- This is the first (root) commit in repo
          commit_id <- createCommitDir msg
          let commit_path = (commitPath commit_id)
          putStrLn ("commit path: " ++ commit_path)

          -- Copy files
          mapM_ (\x -> createDirectoryIfMissing True (commit_path ++ "/" ++ (intercalate "/" (init (splitOn "/" x))))) trackedFiles
          mapM_ (\x -> copyFile (x) (commit_path ++ "/" ++ x)) trackedFiles
          -- Set HEAD

          addCommitChilds head_cid [commit_id]
          setCommitParents commit_id [head_cid]

          setHEAD commit_id
          return "Committed"
        else do
          -- TODO:
          -- (*) check if there are new changes to commit:

          -- Get files (names only) of the HEAD commit
          files_in_head_io <- (listDirectoryRecursive (commitPath head_cid))
          let files_in_head = filter (/= commitMetaName) files_in_head_io
          -- mapM_ (\x->putStrLn(show(x))) files_in_head -- for DEBUG use: print out these files

          -- Show their contents
          -- head_files_content <- mapM (\x -> (getCommitFile head_cid x)) head_files
          -- mapM_ (\x->putStrLn(show(x))) head_files_content

          -- Get files in different states:
          -- new:
          let new_files = filter (\x -> (notElem (x) files_in_head)) trackedFiles
          mapM_ (\x -> putStrLn("New file: " ++ show(x))) new_files

          let files_in_TS = filter (\x -> (elem x files_in_head)) trackedFiles
          -- altered:
          let altered_files = filter (\x -> (unsafePerformIO (checkAltered head_cid (x)))) files_in_TS
          mapM_ (\x -> putStrLn("Altered file: " ++ show(x))) altered_files

          -- unaltered:
          let unaltered_files = filter (\x -> (not (unsafePerformIO (checkAltered head_cid (x))))) files_in_TS
          mapM_ (\x -> putStrLn("Unaltered file: " ++ show(x))) unaltered_files

          -- deleted:
          let deleted_files = filter (\x -> (notElem (x) trackedFiles)) files_in_head

          if ((length new_files) == 0) && ((length altered_files) == 0)
            then do
              putStrLn "No altered or new files: nothing to commit"
              return "Not Committed"
            else do
              -- create a new commit
              commit_id <- createCommitDir msg
              -- set parents and children

              let commit_path = (commitPath commit_id)
              putStrLn ("Commit path: " ++ commit_path)

              -- copy files
              mapM_ (\x -> createDirectoryIfMissing True (commit_path ++ "/" ++ (intercalate "/" (init (splitOn "/" x))))) new_files
              mapM_ (\x -> copyFile (x) (commit_path ++ "/" ++ x)) new_files

              mapM_ (\x -> createDirectoryIfMissing True (commit_path ++ "/" ++ (intercalate "/" (init (splitOn "/" x))))) altered_files
              mapM_ (\x -> copyFile (x) (commit_path ++ "/" ++ x)) altered_files

              mapM_ (\x -> createDirectoryIfMissing True (commit_path ++ "/" ++ (intercalate "/" (init (splitOn "/" x))))) unaltered_files
              mapM_ (\x -> copyFile (x) (commit_path ++ "/" ++ x)) unaltered_files
              -- mapM_ (\x -> createFileLink (x) (commit_path ++ "/" ++ x)) unaltered_files

              -- update parents and children
              addCommitChilds head_cid [commit_id]
              setCommitParents commit_id [head_cid]

              -- update HEAD
              setHEAD commit_id
              return "Committed"

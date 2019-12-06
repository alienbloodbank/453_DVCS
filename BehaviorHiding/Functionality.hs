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

import System.Directory (doesDirectoryExist, getCurrentDirectory, doesFileExist, doesPathExist, listDirectory, copyFile, createDirectoryIfMissing)
import System.Environment
import System.Process
import System.IO.Unsafe
import Data.List
import Data.List.Split

import SoftwareDecision.Concept.Commit (createCommitDir, getCommitFile, commitPath, addCommitChilds, setCommitChilds, setCommitParents, CommitID(..), getCommitMessage)
import SoftwareDecision.Concept.TrackedSet (addFile, removeFile, getTrackedSet, cleanTrackedSet)
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.MetaOrganization (dvcsPath)
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Communication

performInit :: IO String
performInit = do
   doesRepoAlreadyExist <- isRepo
   cd <- getCurrentDirectory
   if doesRepoAlreadyExist then return ("Reinitialized existing dvcs repository in " ++ cd)
   else do
      createRepo
      -- create root commit
      return "Initialized repo"

------------------------------------
performClone :: String -> IO String
performClone repoPath = do
   isLocalPath <- doesPathExist repoPath
   if isLocalPath then do
     doesRepoExist <- doesDirectoryExist $ repoPath ++ "/" ++ dvcsPath
     if doesRepoExist then do
        copyDir repoPath "."
        return "Cloned local repo"
     else return "Local directory doesn't seen to be valid repository"
   else do
     let (hostname, remoteRepo) = break (==':') repoPath
     doesRepoExist <- doesRemoteDirExist hostname ((tail remoteRepo) ++ "/" ++ dvcsPath)
     if doesRepoExist then do
        downloadRemoteDir repoPath
        return "Cloned remote repo"
     else return "Remote directory doesn't seen to be valid repository"

------------------------------------
performAdd :: String -> IO String
performAdd file = do
   doesExist <- isRepo
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else do
     inCD <- doesFileExist file
     trackedFiles <- getTrackedSet
     if not(inCD) then do
       if (file `notElem` trackedFiles) then return "fatal: File does not exist in CD"
       else do
         removeFile file
         return "File removed as its not in CD"
     else do
       addFile file
       return "File added"

------------------------------------
performRemove :: String -> IO String
performRemove file = do
   doesExist <- isRepo
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else do
     trackedFiles <- getTrackedSet
     if (file `notElem` trackedFiles) then return "Error: File not being tracked. Nothing to remove"
     else do
         removeFile file
         return $ file ++ " removed"

------------------------------------
performStatus :: IO String
performStatus = do
   doesExist <- isRepo
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else do
   trackedFiles <- getTrackedSet
   putStrLn "Tracked files:"
   Prelude.mapM_ putStrLn trackedFiles
   putStrLn "\nUntracked files:"
   allFiles <- listDirectory "."
   Prelude.mapM_ putStrLn (allFiles \\ trackedFiles)
   return "success"

checkAltered :: CommitID -> String -> IO Bool
checkAltered head_cid file_name = do
    head_file_c <- (getCommitFile head_cid file_name)
    file_c <- (readFile file_name)
    let if_altered = not (head_file_c == file_c)
    return if_altered

performCommit :: String -> IO String
performCommit msg = do
  trackedFiles <- getTrackedSet

  if (length trackedFiles) == 0
    then return "Nothing to commit: tracked set empty"
    else do
      cleanTrackedSet -- remove files from TS if not in CD
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

          setCommitChilds head_cid [commit_id]
          setCommitParents commit_id [head_cid]

          setHEAD commit_id
          return "committed"
        else do
          -- TODO:
          -- (*) check if there are new changes to commit:

          -- Get files (names only) of the HEAD commit
          files_in_head_io <- (listDirectory (commitPath head_cid))
          let files_in_head = filter (/= "commitMeta.json") files_in_head_io
          -- mapM_ (\x->putStrLn(show(x))) files_in_head -- for DEBUG use: print out these files

          -- Show their contents
          -- head_files_content <- mapM (\x -> (getCommitFile head_cid x)) head_files
          -- mapM_ (\x->putStrLn(show(x))) head_files_content

          -- Get files in different states:
          -- new:
          let new_files = filter (\x -> (notElem (x) files_in_head)) trackedFiles
          mapM_ (\x->putStrLn("new file: " ++ show(x))) new_files

          let files_in_TS = filter (\x -> (elem x files_in_head)) trackedFiles
          -- altered:
          let altered_files = filter (\x -> (unsafePerformIO (checkAltered head_cid (x)))) files_in_TS
          mapM_ (\x->putStrLn("altered file: " ++ show(x))) altered_files

          -- unaltered:
          let unaltered_files = filter (\x -> (not (unsafePerformIO (checkAltered head_cid (x))))) files_in_TS
          mapM_ (\x->putStrLn("unaltered file: " ++ show(x))) unaltered_files

          -- deleted:
          let deleted_files = filter (\x -> (notElem (x) trackedFiles)) files_in_head

          if ((length new_files) == 0) && ((length altered_files) == 0)
            then do
              putStrLn "No altered or new files: nothing to commit"
              return "not committed"
            else do
              -- create a new commit
              commit_id <- createCommitDir msg
              -- set parents and children

              let commit_path = (commitPath commit_id)
              putStrLn ("commit path: " ++ commit_path)

              -- copy files
              mapM_ (\x -> createDirectoryIfMissing True (commit_path ++ "/" ++ (intercalate "/" (init (splitOn "/" x))))) new_files
              mapM_ (\x -> copyFile (x) (commit_path ++ "/" ++ x)) new_files

              mapM_ (\x -> createDirectoryIfMissing True (commit_path ++ "/" ++ (intercalate "/" (init (splitOn "/" x))))) altered_files
              mapM_ (\x -> copyFile (x) (commit_path ++ "/" ++ x)) altered_files

              mapM_ (\x -> createDirectoryIfMissing True (commit_path ++ "/" ++ (intercalate "/" (init (splitOn "/" x))))) unaltered_files
              mapM_ (\x -> copyFile (x) (commit_path ++ "/" ++ x)) unaltered_files
              -- mapM_ (\x -> createFileLink (x) (commit_path ++ "/" ++ x)) unaltered_files

              -- update parents and children
              setCommitChilds head_cid [commit_id]
              setCommitParents commit_id [head_cid]

              -- update HEAD
              setHEAD commit_id
              return "committed"

-- TODO --
------------------------------------
performHeads :: IO String
performHeads = do
  doesExist <- isRepo
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else do
    head <- getHEAD
    if head == (CommitID "root") then return "fatal: no commits in current repository."
    else do
      putStr $ "commit: " ++ (getStr head) ++ "\n"
      message <- getCommitMessage head
      putStr $ "message: " ++ message
      return ""

performDiff :: String -> String -> IO String
performDiff revid1 revid2 = do return "dvcs diff output"

performLog :: IO String
performLog = do return "dvcs log output"

performCheckout :: String -> IO String
performCheckout revid = do return "Checked out"

performCat :: String -> String -> IO String
performCat revid file = do
  doesExist <- isRepo
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else do
    inCD <- doesFileExist file
    if not(inCD) then return "fatal: file does not exist in current directory"
    else do
      cur_file <- getCommitFile (CommitID revid) file
      putStr cur_file
      return ""

performPull :: String -> IO String
performPull repo_path = do return "Pulled"

performPush :: String -> IO String
performPush repo_path = return "Pushed"

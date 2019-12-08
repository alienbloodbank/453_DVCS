module BehaviorHiding.Functionality(
performInit,
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
import System.FilePath.Posix
import Data.List
import Data.List.Split
import Data.Algorithm.Diff

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.TrackedSet
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.MetaOrganization
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Communication

performInit :: IO String
performInit = do
   doesRepoAlreadyExist <- isRepo
   cd <- getCurrentDirectory
   if doesRepoAlreadyExist then return $ "Reinitialized existing dvcs repository in " ++ cd
   else do 
      createRepo
      return $ "Initialized repository in " ++ cd

------------------------------------
performClone :: String -> IO String
performClone repoPath = do
   isLocalPath <- doesPathExist repoPath
   if isLocalPath then do
     doesRepoExist <- doesDirectoryExist $ repoPath ++ "/" ++ dvcsPath
     if doesRepoExist then do
        copyDir repoPath "."
        return "Cloned local repository"
     else return "Local directory is not a valid repository"
   else do
     let (hostname, remoteRepo) = break (==':') repoPath
     doesRepoExist <- doesRemoteDirExist hostname ((tail remoteRepo) ++ "/" ++ dvcsPath)
     if doesRepoExist then do
        downloadRemoteDir repoPath
        return "Cloned remote repository"
     else return "Remote directory is not a valid repository"

------------------------------------
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
         removeFile file
         return "File removed as its not in current directory"
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
         return "File removed"

------------------------------------
performStatus :: IO String
performStatus = do
   doesExist <- isRepo
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else do
   trackedFiles <- getTrackedSet
   putStrLn "Tracked files:"
   mapM_ putStrLn trackedFiles
   putStrLn "\nUntracked files:"
   allFiles <- listDirectory "."
   mapM_ putStrLn ((Data.List.delete ".dvcs"  allFiles) \\ trackedFiles)
   return "\nRepository status"

------------------------------------
checkAltered :: CommitID -> String -> IO Bool
checkAltered head_cid file_name = do
    head_file_c <- (getCommitFile head_cid file_name)
    file_c <- (readFile file_name)
    let if_altered = not (head_file_c == file_c)
    return if_altered

performCommit :: String -> IO String
performCommit msg = do
  doesExist <- isRepo
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else do
  trackedFiles <- getTrackedSet
  if trackedFiles == []
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
          return "Committed"
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
              setCommitChilds head_cid [commit_id]
              setCommitParents commit_id [head_cid]

              -- update HEAD
              setHEAD commit_id
              return "Committed"

------------------------------------
performHeads :: IO String
performHeads = do
  doesExist <- isRepo
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else do
    commit_head <- getHEAD
    if commit_head == (CommitID "root") then return "fatal: no commits in current repository."
    else do
      putStrLn $ "Commit: " ++ (getStr commit_head)
      message <- getCommitMessage commit_head
      putStrLn $ "Message: " ++ message
      time <- getCommitDate commit_head
      putStrLn $ "Time: " ++ time ++ "\n"
      return "Heads shown"

------------------------------------
performDiff :: String -> String -> IO String
performDiff revid1 revid2 = do
  doesExist <- isRepo
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
    else if revid1 == revid2 then return "fatal: ID's are identical"
      else do
        let path1 = commitPath (CommitID revid1)
        let path2 = commitPath (CommitID revid2)
        isPath1 <- doesPathExist path1
        isPath2 <- doesPathExist path2
        if not(isPath1) then return ("fatal: invalid commit id." ++ revid1)
          else if not(isPath2) then return ("fatal: invalid commit id." ++ revid2)
            else do
              files1 <- listDirectory path1
              files2 <- listDirectory path2
              let dFiles = getDiff (Data.List.delete commitMetaName files1) (Data.List.delete commitMetaName files2)
              mapM_ (\snap -> case snap of 
                                       (First f) -> do
                                                      putStrLn f
                                                      putStrLn $ "File not existant in second commit " ++ (takeBaseName path2) ++ "\n"
                                       (Second f) -> do
                                                      putStrLn f
                                                      putStrLn $ "File not existant in first commit " ++ (takeBaseName path1) ++ "\n"
                                       (Both a b) -> do
                                                      putStrLn a
                                                      _ <- system $ "diff " ++ (path1 ++ "/" ++ a) ++ " " ++ (path2 ++ "/" ++ b)
                                                      putStrLn "") dFiles
              return "Diff shown"

--------------------------------------
performLog :: IO String
performLog = do
  doesExist <- isRepo
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else do
    commit_head <- getHEAD
    if commit_head == (CommitID "root") then return "fatal: no commits in current repository."
    else do
      commit_list <- getUpToHead
      let com_list = tail commit_list 
      putStrLn "(HEAD)"
      mapM_ (\com -> do
                     commit_message <- getCommitMessage com
                     commit_date <- getCommitDate com
                     putStrLn $ "Commit: " ++ (getStr com) ++ "\n" ++
                                "Message: " ++ commit_message ++ "\n" ++
                                "Time: " ++ commit_date ++ "\n") com_list
      return "Commit history"

--------------------------------------
performCheckout :: String -> IO String
performCheckout revid = do
  doesExist <- isRepo
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else do
    let commit_path = commitPath (CommitID revid)
    isPath <- doesPathExist commit_path
    if not(isPath) then return "fatal: invalid commit id."
    else do
      setHEAD (CommitID revid)
      return ("Head successfully changed to " ++ revid)

--------------------------------------
performCat :: String -> String -> IO String
performCat revid file = do
  doesExist <- isRepo
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else do
    let commit_path = commitPath (CommitID revid)
    isPath <- doesPathExist commit_path
    if not(isPath) then return "fatal: invalid commit id."
    else do
      cur_file <- getCommitFile (CommitID revid) file
      return cur_file

-- TODO --
--------------------------------------
performPull :: String -> IO String
performPull repo_path = do return "Pulled"

performPush :: String -> IO String
performPush repo_path = return "Pushed"

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
   Prelude.mapM_ putStrLn ((Data.List.delete ".dvcs"  allFiles) \\ trackedFiles)
   return "success"

------------------------------------
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
      -- time <- getCommitDate head
      putStrLn $ "Message: " ++ message
      time <- getCommitDate commit_head
      putStrLn $ "Time: " ++ time ++ "\n"
      return "Heads shown"

------------------------------------
performSnapshotDiff :: [Diff String] -> String -> String -> IO ()
performSnapshotDiff dFiles compath1 compath2 = do
  if dFiles == [] then return ()
  else do
    let (hdiff:rest) = dFiles
    case hdiff of (First f) -> do 
                                 putStrLn f
                                 putStrLn "File not existant in second commit\n"
                  (Second f) -> do
                                 putStrLn f
                                 putStrLn "File not existant in first commit\n"
                  (Both a b) -> do
                               putStrLn a
                               _ <- system $ "diff " ++ (compath1 ++ "/" ++ a) ++ " " ++ (compath2 ++ "/" ++ b)
                               putStrLn ""
    performSnapshotDiff rest compath1 compath2

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
              performSnapshotDiff (getDiff (Data.List.delete commitMetaName files1) (Data.List.delete commitMetaName files2)) path1 path2
              return "Diff shown"

--------------------------------------
logHelper :: [CommitID] -> IO [(String, String, String)]
logHelper commit_list = do
  if commit_list == [] then return []
  else do
    let (first:rest) = commit_list
    commit_message <- getCommitMessage first
    commit_date <- getCommitDate first
    let (cid, message, time) = (getStr first, commit_message, commit_date)
    partial <- logHelper rest
    return ((cid, message, time):partial)


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
      commit_info <- logHelper com_list
      let (x, y, z):rest = commit_info
      let u_commit_info = (x ++ " (HEAD)", y, z):rest
      mapM_ (\(x,y,z) -> putStrLn $ "Commit: " ++ x ++ "\n" ++
                                    "Message: " ++ y ++ "\n" ++
                                    "Time: " ++ z ++ "\n") commit_info
      return "Log shown"

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
      putStr cur_file
      return "Cat output"

-- TODO --
--------------------------------------
performPull :: String -> IO String
performPull repo_path = do return "Pulled"

performPush :: String -> IO String
performPush repo_path = return "Pushed"

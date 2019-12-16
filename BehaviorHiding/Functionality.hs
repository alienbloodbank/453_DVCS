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

import System.Directory (withCurrentDirectory,
                         removeFile,
                         setCurrentDirectory,
                         removeDirectoryRecursive,
                         doesDirectoryExist, getCurrentDirectory, doesFileExist, doesPathExist, listDirectory, copyFile, createDirectoryIfMissing)
import System.Environment
import System.Process
import System.IO.Unsafe
import System.FilePath.Posix
import Data.List
import Data.List.Split
import Data.Algorithm.Diff
import Control.Monad
import Control.Monad.ListM

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.MetaOrganization
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Communication


--- HELPERS ---
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive filePath = do
   files <- listDirectory filePath
   let filteredFiles = filter (\f -> not . or $ [f == ".git", f == ".dvcs"]) files
   filesOnly <- foldM (\acc x -> do
                               isFile <- doesFileExist $ filePath ++ "/" ++ x
                               if isFile then return (x:acc)
                               else do
                                   files_ <- listDirectoryRecursive $ filePath ++ "/" ++ x
                                   let withPath = map (\f -> x ++ "/" ++ f) files_
                                   return $ withPath ++ acc) [] filteredFiles
   return filesOnly

checkAltered :: CommitID -> String -> IO Bool
checkAltered head_cid file_name = do
    head_file_c <- (getCommitFile head_cid file_name)
    file_c <- (readFile file_name)
    let if_altered = not (head_file_c == file_c)
    return if_altered

checkUnstashed :: IO Bool
checkUnstashed = do
   head_cid <- getHEAD
   cleanTrackedSet

   files_in_head_io <- (listDirectoryRecursive (commitPath head_cid))
   let files_in_head = filter (/= commitMetaName) files_in_head_io

   trackedFiles <- getTrackedSet
   -- Get files in different states:
   -- new:
   let are_new_files = any (\x -> (notElem (x) files_in_head)) trackedFiles

   let files_in_TS = filter (\x -> (elem x files_in_head)) trackedFiles
   -- altered:
   are_altered_files <- anyM (\x -> checkAltered head_cid (x)) files_in_TS

   return $ are_new_files || are_altered_files

mergepull :: IO String
mergepull = do
   c_pid <- getPID
   r_pid <- getRemotePID
   if r_pid /= c_pid then return "error: Invalid remote"
   else do
      mrca_id <- getMRCA >>= (\mrca -> case mrca of (Just id) -> return id
                                                    Nothing -> return (CommitID "root"))
      hid <- getHEAD
      remote_coms <- getUpToRemoteHeadRecursive [mrca_id] >>= (\r -> return (tail r))
      if hid == mrca_id then do
         -- Fast Forward merge
         if remote_coms == [] then return "Everything up-to-date"
         else do
           -- copying the remote snapshots to the local repo
           mapM_ (\c -> copyDir objectPath (remoteCommitPath c)) remote_coms
           -- update parents and children
           mrca_new_childs <- getRemoteCommitChilds mrca_id

           mapM_ (\c -> addCommitChilds hid [c]) mrca_new_childs

           trackedFiles <- getTrackedSet

           let new_head = last remote_coms
           let commit_path = commitPath new_head
           cwd <- getCurrentDirectory

           -- Updating trackedset and copying files from new head to current folder
           mapM_ (\f -> do
                           System.Directory.removeFile f
                           TS.removeFile f) trackedFiles

           files_in_rev <- listDirectoryRecursive commit_path >>= (\f -> return $ filter (/= commitMetaName) f)

           withCurrentDirectory commit_path $ do
                                                mapM_ (\x -> do
                                                                createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" x))
                                                                copyFile (x) (cwd ++ "/" ++ x)) files_in_rev
           mapM_ (\x -> TS.addFile x) files_in_rev
           setHEAD new_head
           return "Successfully pulled"
      else do
         -- 3-way merge
         if remote_coms == [] then return "error: Local is ahead of remote"
         else do
           -- copying the remote snapshots to the local repo
           mapM_ (\c -> copyDir objectPath (remoteCommitPath c)) remote_coms

           -- add a link between mrca and first commit in remote chain
           mrca_new_childs <- getRemoteCommitChilds mrca_id
           mapM_ (\c -> addCommitChilds mrca_id [c]) mrca_new_childs

           -- add a merge commit and update its links
           rhid <- getRemoteHEAD -- should be equal to (last remote_coms)

           merge_commit_id <- createCommitDir $ "Merge Committed " ++ (getStr hid) ++ "/" ++ (getStr hid)
           let merge_commit_path = commitPath merge_commit_id

           addCommitParents merge_commit_id [hid, rhid]
           addCommitChilds hid [merge_commit_id]
           addCommitChilds rhid [merge_commit_id]

           -- remove files in the tracked set
           trackedFiles <- getTrackedSet
           mapM_ (\f -> do
                           System.Directory.removeFile f
                           TS.removeFile f) trackedFiles

           let path1 = commitPath hid
           let path2 = commitPath rhid

           -- list of files in the 2 'heads'
           files1 <- listDirectoryRecursive path1 >>= (\fs -> return $ Data.List.delete commitMetaName fs)
           files2 <- listDirectoryRecursive path2 >>= (\fs -> return $ Data.List.delete commitMetaName fs)
           let dFiles = getDiff files1 files2

           cwd <- getCurrentDirectory

           merge_conflicts <- foldM (\acc df -> case df of (First f) -> do
                                                                           withCurrentDirectory path1 $ do
                                                                                  createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" f))
                                                                                  copyFile f (cwd ++ "/" ++ f)
                                                                           TS.addFile f
                                                                           return acc
                                                           (Second f) -> do
                                                                           withCurrentDirectory path2 $ do
                                                                                  createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" f))
                                                                                  copyFile f (cwd ++ "/" ++ f)
                                                                           TS.addFile f
                                                                           return acc
                                                           (Both a b) -> do -- Optional TODO: git based smart merge
                                                                           contents1 <- getCommitFile hid a
                                                                           contents2 <- getCommitFile hid b
                                                                           if contents1 == contents2 then do
                                                                             withCurrentDirectory path1 $ do
                                                                                  createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" a))
                                                                                  copyFile a (cwd ++ "/" ++ a)
                                                                             TS.addFile a
                                                                             return acc
                                                                           else do
                                                                             withCurrentDirectory path1 $ do
                                                                                  createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" a))
                                                                                  copyFile a (cwd ++ "/" ++ a ++ "_CURRENT")
                                                                             withCurrentDirectory path2 $ do
                                                                                     createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" b))
                                                                                     copyFile b (cwd ++ "/" ++ b ++ "_OTHER")
                                                                             return $ acc ++ [a ++ "_CURRENT", b ++ "_OTHER"]) [] dFiles
           setHEAD merge_commit_id
           if merge_conflicts == [] then return "Successfully pulled"
           else do
              putStrLn "MERGE CONFLICT!"
              putStrLn "Resolve these files, add them and then commit manually"
              mapM_ putStrLn merge_conflicts
              -- creating a locked directory to indicate merge conflict
              createDirectoryIfMissing False ".LOCKED"
              return "Pull Incomplete"


mergepush :: IO String
mergepush = do
   c_pid <- getPID
   r_pid <- getRemotePID
   if r_pid /= c_pid then return "error: Invalid remote"
   else do
      mrca_id <- getMRCA >>= (\mrca -> case mrca of (Just id) -> do return id
                                                    Nothing -> do return (CommitID "root"))
      rhid <- getRemoteHEAD
      if rhid == mrca_id then do
         -- Fast Forward merge
         coms <- getUpToHeadRecursive [mrca_id] >>= (\r -> return (tail r))
         if coms == [] then return "Everything up-to-date"
         else do
           -- copying the local snapshots to the remote repo
           mapM_ (\c -> copyDir (remoteLoc ++ "/" ++ objectPath) (commitPath c)) coms
           -- update parents and children
           mrca_new_childs <- getCommitChilds mrca_id

           mapM_ (\c -> addRemoteCommitChilds rhid [c]) mrca_new_childs

           trackedFiles <- getRemoteTrackedSet

           let new_rhead = last coms
           let rcommit_path = remoteCommitPath new_rhead
           cwd <- getCurrentDirectory

           -- Updating trackedset and copying files from new head to current folder

           withCurrentDirectory remoteLoc (mapM_ (\f -> do
                                                           System.Directory.removeFile f
                                                           TS.removeFile f) trackedFiles)

           files_in_rev <- listDirectoryRecursive rcommit_path >>= (\f -> return $ filter (/= commitMetaName) f)

           withCurrentDirectory rcommit_path (do
                                                mapM_ (\x -> do
                                                                createDirectoryIfMissing True ((cwd ++ "/" ++ remoteLoc) ++ "/" ++ (intercalate "/" $ init $ splitOn "/" x))
                                                                copyFile (x) ((cwd ++ "/" ++ remoteLoc) ++ "/" ++ x)) files_in_rev)
           withCurrentDirectory remoteLoc (mapM_ (\x -> TS.addFile x) files_in_rev)
           setRemoteHEAD new_rhead
           return "Successfully pushed"
      else do return "fatal: remote has changed. Please pull first"

-----------------------------------
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
performClone repo_path_impure = do
   let repoPath = normalise $ dropTrailingPathSeparator repo_path_impure
   isLocalPath <- doesPathExist repoPath
   if isLocalPath then do
     doesRepoExist <- doesDirectoryExist $ repoPath ++ "/" ++ dvcsPath
     if doesRepoExist then do
        copyDir "." repoPath
        let repo_name = takeBaseName repoPath
        -- Removing files not in the tracked set because clone copies everything
        withCurrentDirectory repo_name (do
                                          trackedSet <- getTrackedSet
                                          allFiles <- listDirectoryRecursive "."
                                          mapM_ (\f -> when (f `notElem` trackedSet) (System.Directory.removeFile f)) allFiles)
        return "Cloned local repository"
     else return "Local directory is not a valid repository"
   else do
     let (_, _:repo) = break (==':') repoPath
     downloadRemoteDir repoPath
     let repo_name = takeBaseName repo
     doesRepoExist <- doesDirectoryExist $ repo_name ++ "/" ++ dvcsPath
     if doesRepoExist then do
        -- Removing files not in the tracked set because clone copies everything
        withCurrentDirectory repo_name (do
                                          trackedSet <- getTrackedSet
                                          allFiles <- listDirectoryRecursive "."
                                          mapM_ (\f -> when (f `notElem` trackedSet) (System.Directory.removeFile f)) allFiles)
        return "Cloned remote repository"
     else do
        removeDirectoryRecursive repo_name
        return "Remote directory is not a valid repository"

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
         TS.removeFile file
         return "File removed as its not in current directory"
     else do
       if file `elem` trackedFiles then return "File already tracked"
       else do
         addFile file
         return "File added"

------------------------------------
performRemove :: String -> IO String
performRemove file = do
   doesExist <- isRepo
   isLocked <- doesDirectoryExist ".LOCKED"
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else if isLocked then return "fatal: Please resolve conflicts and then commit them"
   else do
     trackedFiles <- getTrackedSet
     if (file `notElem` trackedFiles) then return "Error: File not being tracked. Nothing to remove"
     else do
         TS.removeFile file
         return "File removed"

------------------------------------
performStatus :: IO String
performStatus = do
   doesExist <- isRepo
   isLocked <- doesDirectoryExist ".LOCKED"
   if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
   else if isLocked then return "fatal: Please resolve conflicts and then commit them"
   else do
   -- Not using cleanTrackedSet here as its the responsibility of commit
   trackedFiles <- getTrackedSet
   trackedExistingFiles <- filterM (\x -> doesFileExist x) trackedFiles
   unless (trackedExistingFiles == []) (do
                                           putStrLn "Tracked files"
                                           mapM_ putStrLn trackedExistingFiles
                                           putStrLn "")
   let deletedFiles = trackedFiles \\ trackedExistingFiles
   unless (deletedFiles == []) (do
                                   putStrLn "Tracked files that no longer exist!"
                                   mapM_ putStrLn deletedFiles
                                   putStrLn "")
   commit_head <- getHEAD
   unless (commit_head == (CommitID "root")) (do
                                                 let com_path = commitPath commit_head
                                                 commitedFiles <- listDirectoryRecursive com_path
                                                 alteredFiles <- filterM (\f -> do
                                                                              if (f `notElem` commitedFiles) then return False
                                                                              else do (checkAltered commit_head f) >>= \x -> return x) trackedExistingFiles
                                                 unless (alteredFiles == []) (do
                                                                                 putStrLn "Altered files from last commit"
                                                                                 mapM_ putStrLn alteredFiles
                                                                                 putStrLn ""))
   allFiles <- listDirectoryRecursive "."
   let untrackedFiles = allFiles \\ trackedFiles
   case untrackedFiles of [] -> putStrLn "Nothing is untracked!\n"
                          _ -> do
                                  putStrLn "Untracked files"
                                  mapM_ putStrLn untrackedFiles
                                  putStrLn ""
   return "Repository status"

------------------------------------
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
    return "Committed"
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

------------------------------------
performHeads :: IO String
performHeads = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
  else do
    commit_head <- getHEAD
    if commit_head == (CommitID "root") then return "fatal: no commits in current repository."
    else do
      putStrLn $ "Commit: " ++ (getStr commit_head)
      message <- getCommitMessage commit_head
      putStrLn $ "Message: " ++ message
      time <- getCommitDate commit_head
      putStrLn $ "Time: " ++ (show time) ++ "\n"
      return "Heads shown"

------------------------------------
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
        if not(isPath1) then return ("fatal: invalid commit id." ++ revid1)
          else if not(isPath2) then return ("fatal: invalid commit id." ++ revid2)
            else do
              files1 <- listDirectoryRecursive path1 >>= (\fs -> return $ Data.List.delete commitMetaName fs)
              files2 <- listDirectoryRecursive path2 >>= (\fs -> return $ Data.List.delete commitMetaName fs)
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
                                                      callCommand $ "diff " ++ (path1 ++ "/" ++ a) ++ " " ++ (path2 ++ "/" ++ b)
                                                      putStrLn "") dFiles
              return "Diff shown"

--------------------------------------
performLog :: IO String
performLog = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
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
                                "Time: " ++ (show commit_date) ++ "\n") (reverse com_list)
      return "Commit history"

--------------------------------------
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

        files_in_rev <- listDirectoryRecursive commit_path >>= (\f -> return $ filter (/= commitMetaName) f)

        withCurrentDirectory commit_path (do
                                            mapM_ (\x -> do
                                                           createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" x))
                                                           copyFile (x) (cwd ++ "/" ++ x)) files_in_rev)
        mapM_ (\x -> TS.addFile x) files_in_rev
        setHEAD (CommitID revid)
        return ("Head successfully changed to " ++ revid)

--------------------------------------
performCat :: String -> String -> IO String
performCat revid file = do
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
  else do
    let commit_path = commitPath (CommitID revid)
    isPath <- doesPathExist commit_path
    if not(isPath) then return "fatal: invalid commit id."
    else do
      cur_file <- getCommitFile (CommitID revid) file
      return cur_file

---------------------------------------
-- TODO: 3 Way merge --
performPull :: String -> IO String
performPull repo_path_impure = do
  let repo_path = normalise $ dropTrailingPathSeparator repo_path_impure
  doesExist <- isRepo
  isLocked <- doesDirectoryExist ".LOCKED"
  if not(doesExist) then return "fatal: not a dvcs repository .dvcs"
  else if isLocked then return "fatal: Please resolve conflicts and then commit them"
  else do
  isUnStashed <- checkUnstashed
  if isUnStashed then return "error: Please commit your changes or revert them before you pull"
  else do
   isLocalPath <- doesPathExist repo_path
   if isLocalPath then do
     doesRepoExist <- doesDirectoryExist $ repo_path ++ "/" ++ dvcsPath
     if doesRepoExist then do
        copyRepo (LocalPath repo_path)
        msg <- mergepull
        removeDirectoryRecursive remoteLoc
        return msg
     else return "Local directory is not a valid repository"
   else do
     let (_, _:repo) = break (==':') repo_path
     withCurrentDirectory tempPath (downloadRemoteDir repo_path)
     renameDir (tempPath ++ "/" ++ (takeBaseName repo)) remoteLoc
     doesRepoExist <- doesDirectoryExist $ remoteLoc ++ "/" ++ dvcsPath
     if doesRepoExist then do
        msg <- mergepull
        removeDirectoryRecursive remoteLoc
        return msg
     else do
        removeDirectoryRecursive remoteLoc
        return "Remote directory is not a valid repository"

----------------------------------------
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
        msg <- mergepush
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
        msg <- mergepush
        withCurrentDirectory remoteLoc (uploadRemoteDir repo_path)
        removeDirectoryRecursive remoteLoc
        return msg
     else do
        removeDirectoryRecursive remoteLoc
        return "Remote directory is not a valid repository"

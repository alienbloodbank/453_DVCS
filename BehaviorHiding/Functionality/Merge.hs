module BehaviorHiding.Functionality.Merge(smartMerge, mergePull, mergePush) where

import System.Directory (withCurrentDirectory,
                         removeFile,
                         getCurrentDirectory,
                         doesFileExist,
                         copyFile,
                         createDirectoryIfMissing)

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


-- https://www.drdobbs.com/tools/three-way-merging-a-look-under-the-hood/240164902 --
smartMerge :: String -> String -> String -> Maybe String
smartMerge mrca file1 file2 =
   let new_file = [ if line1 == line2 then line1
                    else if line1 == linem then line2
                    else if line2 == linem then line1
                    else (conflicted_line ++ linem) | i <- [0 .. max_size - 1],
                    let (line1, line2, linem) = (file1_lines !! i, file2_lines !! i, mrca_lines !! i)] in
   if (conflicted_line `elem` new_file) then Nothing else Just (unlines new_file)
   where mrca_lines = pad mrca
         file1_lines = pad file1
         file2_lines = pad file2
         max_size = maximum $ map (length . lines) [mrca, file1, file2]
         pad file = (lines file) ++ (replicate (max_size - (length file)) "")
         conflicted_line = ">>>>>>>>> MERGE CONFLICT!!\n"

mergePull :: IO String
mergePull = do
   c_pid <- getPID
   r_pid <- getRemotePID
   if r_pid /= c_pid then return "error: Invalid remote"
   else do
      mrca_id <- getMRCA
      hid <- getHEAD
      remote_coms <- getUpToRemoteHeadRecursive [mrca_id] >>= return . tail
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

           files_in_rev <- listDirectoryRecursive commit_path >>= return . filter (/= commitMetaName)

           withCurrentDirectory commit_path $ do
                                                mapM_ (\x -> do
                                                                createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" x))
                                                                copyFile (x) (cwd ++ "/" ++ x)) files_in_rev
           mapM_ TS.addFile files_in_rev
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

           merge_commit_id <- createCommitDir $ "Merge Committed " ++ (getStr hid) ++ "/" ++ (getStr rhid)
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
           files1 <- listDirectoryRecursive path1 >>= return . filter (/= commitMetaName)
           files2 <- listDirectoryRecursive path2 >>= return . filter (/= commitMetaName)
           let dFiles = getDiff files1 files2

           cwd <- getCurrentDirectory

           let addFileToMergeCommit path merge_path f = do
                                                           withCurrentDirectory path $ do
                                                                  createDirectoryIfMissing True (cwd ++ "/" ++ (intercalate "/" $ init $ splitOn "/" f))
                                                                  copyFile f (cwd ++ "/" ++ f)
                                                           withCurrentDirectory merge_path $ do
                                                                  createDirectoryIfMissing True (intercalate "/" $ init $ splitOn "/" f)
                                                                  copyFile (cwd ++ "/" ++ f) f
                                                           TS.addFile f

           merge_conflicts <- foldM (\acc df -> case df of (First f) -> do addFileToMergeCommit path1 merge_commit_path f
                                                                           return acc
                                                           (Second f) -> do addFileToMergeCommit path2 merge_commit_path f
                                                                            return acc
                                                           (Both a b) -> do
                                                                           contents1 <- getCommitFile hid a
                                                                           contents2 <- getCommitFile rhid b
                                                                           doesExistinMrca <- doesFileExist $ (commitPath mrca_id) ++ "/" ++ a
                                                                           contentsm <- if doesExistinMrca then (getCommitFile mrca_id a) else return ""
                                                                           if contents1 == contents2 then do
                                                                             addFileToMergeCommit path1 merge_commit_path a
                                                                             return acc
                                                                           else do
                                                                             let results = smartMerge contentsm contents1 contents2
                                                                             clash <- case results of (Just new_contents) -> do
                                                                                                      -- git based smart merge (Experimental)
                                                                                                       addFileToMergeCommit path1 merge_commit_path a
                                                                                                       writeFile a new_contents
                                                                                                       writeFile (merge_commit_path ++ "/" ++ a) new_contents
                                                                                                       return []
                                                                                                      Nothing -> do
                                                                                                       withCurrentDirectory path1 $ do
                                                                                                         createDirectoryIfMissing True (cwd ++ "/" ++
                                                                                                          (intercalate "/" $ init $ splitOn "/" a))
                                                                                                         copyFile a (cwd ++ "/" ++ a ++ "_CURRENT")
                                                                                                       withCurrentDirectory path2 $ do
                                                                                                         createDirectoryIfMissing True (cwd ++ "/" ++
                                                                                                          (intercalate "/" $ init $ splitOn "/" b))
                                                                                                         copyFile b (cwd ++ "/" ++ b ++ "_OTHER")
                                                                                                       return $ [a ++ "_CURRENT", b ++ "_OTHER"]
                                                                             return $ acc ++ clash) [] dFiles
           setHEAD merge_commit_id
           if merge_conflicts == [] then return "Successfully pulled"
           else do
              putStrLn "MERGE CONFLICT!"
              putStrLn "Resolve these files, add them and then commit manually"
              mapM_ putStrLn merge_conflicts
              -- creating a locked directory to indicate merge conflict
              createDirectoryIfMissing False ".LOCKED"
              return "Pull Incomplete"


mergePush :: IO String
mergePush = do
   c_pid <- getPID
   r_pid <- getRemotePID
   if r_pid /= c_pid then return "error: Invalid remote"
   else do
      mrca_id <- getMRCA
      rhid <- getRemoteHEAD
      if rhid == mrca_id then do
         -- Fast Forward merge
         coms <- getUpToHeadRecursive [mrca_id] >>= return . tail
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

           withCurrentDirectory remoteLoc $ mapM_ (\f -> do
                                                           System.Directory.removeFile f
                                                           TS.removeFile f) trackedFiles

           files_in_rev <- listDirectoryRecursive rcommit_path >>= return . filter (/= commitMetaName)

           withCurrentDirectory rcommit_path $ do
                                                mapM_ (\x -> do
                                                                createDirectoryIfMissing True ((cwd ++ "/" ++ remoteLoc) ++ "/" ++ (intercalate "/" $ init $ splitOn "/" x))
                                                                copyFile (x) ((cwd ++ "/" ++ remoteLoc) ++ "/" ++ x)) files_in_rev
           withCurrentDirectory remoteLoc (mapM_ TS.addFile files_in_rev)
           setRemoteHEAD new_rhead
           return "Successfully pushed"
      else do return "fatal: remote has changed. Please pull first"

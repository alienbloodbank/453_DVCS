module BehaviorHiding.Functionality.Helpers(checkAltered, checkUnstashed) where

import Control.Monad.ListM

import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Utility.DvcsInterface


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

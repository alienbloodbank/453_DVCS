{-# LANGUAGE DeriveGeneric #-}

module SoftwareDecision.Concept.TrackedSet (addFile, removeFile, getTrackedSet) where

import SoftwareDecision.Concept.Repo
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List as List
import System.Directory (copyFile, doesFileExist)

addFile :: String -> IO ()
addFile fileName = do
   putStrLn fileName
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   let new = RepoMetadata {pid = p, head_ = h, ts = List.nub $ fileName:files}
   B.writeFile "./.dvcs/repometadatatemp.json" (encode new)
   copyFile "./.dvcs/repometadatatemp.json" "./.dvcs/repometadata.json"

removeFile :: String -> IO ()
removeFile fileName = do
   putStrLn fileName
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   let new = RepoMetadata {pid = p, head_ = h, ts = List.filter (\x -> x /= fileName) files}
   B.writeFile "./.dvcs/repometadatatemp.json" (encode new)
   copyFile "./.dvcs/repometadatatemp.json" "./.dvcs/repometadata.json"

getTrackedSet :: IO [String]
getTrackedSet = do
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return files

-- HELPER FUNCTION 1 --
removeNonExistantFiles :: [String] -> IO ()
removeNonExistantFiles files = do
   if files == [] then return ()
   else do
      let (file:res) = files
      doesExist <- doesFileExist file
      if doesExist then return ()
      else do
        removeFile file
        removeNonExistantFiles res

cleanTrackedSet :: IO ()
cleanTrackedSet = do
   trackedFiles <- getTrackedSet
   removeNonExistantFiles trackedFiles
   

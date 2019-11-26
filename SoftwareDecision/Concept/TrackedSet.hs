{-# LANGUAGE DeriveGeneric #-}

module SoftwareDecision.Concept.TrackedSet (addFile, removeFile, getTrackedSet) where

import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.MetaOrganization (repoMetaPath)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List as List
import System.Directory (copyFile, doesFileExist)

addFile :: String -> IO ()
addFile fileName = do
   putStrLn fileName
   contents <- ((decodeFileStrict repoMetaPath) :: IO (Maybe RepoMetadata))
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = contents
   let new = RepoMetadata {pid = p, head_ = h, ts = List.nub $ fileName:files}
   B.writeFile repoMetaPath (encode new)

removeFile :: String -> IO ()
removeFile fileName = do
   putStrLn fileName
   contents <- ((decodeFileStrict repoMetaPath) :: IO (Maybe RepoMetadata))
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = contents
   let new = RepoMetadata {pid = p, head_ = h, ts = List.filter (\x -> x /= fileName) files}
   B.writeFile repoMetaPath (encode new)

getTrackedSet :: IO [String]
getTrackedSet = do
   contents <- ((decodeFileStrict repoMetaPath) :: IO (Maybe RepoMetadata))
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = contents
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
   

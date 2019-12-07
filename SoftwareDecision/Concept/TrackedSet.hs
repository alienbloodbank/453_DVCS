{-# LANGUAGE DeriveGeneric #-}

module SoftwareDecision.Concept.TrackedSet (addFile, removeFile, getTrackedSet, cleanTrackedSet) where

import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.MetaOrganization
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List as List
import System.Directory (copyFile, doesFileExist)

readTS :: IO [String]
readTS = do
   contents <- B.readFile repoMetaPath
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata   
   return files
    
writeTS :: [String] -> IO ()
writeTS trackedSet = do
   contents <- B.readFile repoMetaPath
   let (Just (RepoMetadata {pid = p, head_ = h, ts = _})) = (decode contents) :: Maybe RepoMetadata
   let new = RepoMetadata {pid = p, head_ = h, ts = trackedSet}
   
   let tmpFilePath = "./.dvcs/repometadatatemp.json"
   B.writeFile tmpFilePath (encode new)
   copyFile tmpFilePath repoMetaPath

addFile :: String -> IO ()
addFile fileName = do
   trackedSet <- readTS
   let newTrackedSet = List.nub $ fileName : trackedSet
   writeTS newTrackedSet

removeFile :: String -> IO ()
removeFile fileName = do
   trackedSet <- readTS
   let newTrackedSet = List.delete fileName trackedSet
   writeTS newTrackedSet

getTrackedSet :: IO [String]
getTrackedSet = do
   trackedSet <- readTS
   return trackedSet

cleanTrackedSet :: IO ()
cleanTrackedSet = do
   trackedSet <- readTS
   mapM_ (\file -> do
                    doesExist <- doesFileExist file
                    if doesExist then return ()
                    else removeFile file) trackedSet
   

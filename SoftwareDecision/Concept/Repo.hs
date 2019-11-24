{-# LANGUAGE DeriveGeneric #-}
module SoftwareDecision.Concept.Repo(RepoMetadata(..), createRepo) where

import GHC.Generics
import Data.Aeson

import System.Exit
import System.Directory (createDirectory, copyFile, doesDirectoryExist)
import Test.RandomStrings (randomString, onlyAlphaNum, randomASCII)
import qualified Data.ByteString.Lazy as B
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Concept.Commit
import System.FilePath.Posix

data RepoMetadata = RepoMetadata {
      pid :: String
     ,head_ :: CommitID
     ,ts :: [String]
    } deriving (Generic, Show)

instance FromJSON RepoMetadata
instance ToJSON RepoMetadata

generateRepoID :: IO String
generateRepoID = do 
   pid <- randomString (onlyAlphaNum randomASCII) 20
   return pid

createRepo :: IO ()
createRepo = do
   dirExist <- doesDirectoryExist "./.dvcs"
   if not dirExist
   then do 
      createDirectory $ "./." ++ dvcsName
      createDirectory $ "./." ++ dvcsName ++ "/info"
      createDirectory $ "./." ++ dvcsName ++"/snapshot"
      createDirectory $ "./." ++ dvcsName ++"/temp"
      p <- generateRepoID
      let r = RepoMetadata {pid = p, head_ = (CommitID "root"), ts = []} 
      B.writeFile "./.dvcs/repometadata.json" (encode r)
      writeFile "./.dvcs/repometadatatemp.json" "" 
      createRootDir
   else return ()

getPID :: IO String
getPID = do
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return p

setHEAD :: CommitID -> IO ()
setHEAD commitId = do
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = _, ts = files})) = (decode contents) :: Maybe RepoMetadata
   let new = RepoMetadata {pid = p, head_ = commitId, ts = files}
   B.writeFile "./.dvcs/repometadatatemp.json" (encode new)
   copyFile "./.dvcs/repometadatatemp.json" "./.dvcs/repometadata.json"

getHEAD :: IO CommitID
getHEAD = do
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return h 

getLocalLeaf :: IO CommitID
getLocalLeaf = do
   head <- getHEAD
   childs <- (getCommitChilds head)
   if (length childs) == 0
    then return head
    else do 
      nx <- getNext head
      return nx

getNext :: CommitID -> IO CommitID
getNext x = do
   childs <- (getCommitChilds x)
   if (length childs) == 0
    then return x
    else do
      nx <- getNext $ head childs
      return nx

-- currently only a directory in the same machine is allowed
data RepoPath = LocalPath FilePath deriving (Show)

tempPath = "./." ++ dvcsName ++ "/temp"
remoteLoc = "./." ++ dvcsName ++ "/temp/remote"

copyRepo :: RepoPath -> IO ()
copyRepo (LocalPath p) = do 
   copyDir tempPath p
   renameDir (tempPath ++ "/" ++ (takeBaseName p)) remoteLoc

remoteCommitMetaPath :: CommitID -> String
remoteCommitMetaPath cid = remoteLoc ++ "/" ++ objectPath ++ "/" 
   ++ (getStr cid) ++ "/" ++ commitMetaName

getRemotePID :: IO String
getRemotePID = do
   contents <- B.readFile $ remoteLoc ++ "/.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return p

getRemoteHEAD :: IO CommitID
getRemoteHEAD = do
   contents <- B.readFile $ remoteLoc ++ "/.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return h 

getRemoteTrackedSet :: IO [String]
getRemoteTrackedSet = do
   contents <- B.readFile $ remoteLoc ++ "/.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return files

getRemoteCommitChilds :: CommitID -> IO [CommitID]
getRemoteCommitChilds cid = getCommitChildsWithPath $ remoteCommitMetaPath cid

getRemoteCommitParents :: CommitID -> IO [CommitID]
getRemoteCommitParents cid = getCommitParentsWithPath $ remoteCommitMetaPath cid

getRemoteLeaf :: IO CommitID
getRemoteLeaf = do 
   head <- getRemoteHEAD
   childs <- (getRemoteCommitChilds head)
   if (length childs) == 0
    then return head
    else do 
      nx <- getRemoteNext head
      return nx

getRemoteNext :: CommitID -> IO CommitID
getRemoteNext x = do
   childs <- (getRemoteCommitChilds x)
   if (length childs) == 0
    then return x
    else do
      nx <- getRemoteNext $ head childs
      return nx
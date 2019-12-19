{-# LANGUAGE DeriveGeneric #-}
module SoftwareDecision.Concept.Repo(RepoMetadata(..), createRepo, isRepo,
  getRemoteLeaf, getLocalLeaf, getHEAD, getRemoteHEAD, getPID, getRemotePID,
  RepoPath(..), getMRCA, copyRepo, getRemoteTrackedSet, getRemoteCommitChilds,
  getRemoteCommitParents, setHEAD, getUpToHead, getUpToRemoteHeadRecursive, remoteCommitPath,
  getUpToHeadRecursive, setRemoteCommitChilds, setRemoteCommitParents, setRemoteHEAD,
  addRemoteCommitChilds, addRemoteCommitParents, getUpToRootRecursive) where

import GHC.Generics
import Data.Aeson
import Data.Time
import Data.List
import qualified Data.Set as S
import Control.Monad.ListM
import Control.Monad
import System.Exit
import System.Directory (createDirectory, copyFile, doesDirectoryExist)
import Test.RandomStrings (randomString, onlyAlphaNum, randomASCII)
import qualified Data.ByteString.Lazy as B
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Concept.MetaOrganization
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Communication
import System.FilePath.Posix

debug = False

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

isRepo :: IO Bool
isRepo = doesDirectoryExist dvcsPath

createRepo :: IO ()
createRepo = do
   createDirectory dvcsPath
   createDirectory objectPath
   createDirectory metaPath
   createDirectory tempPath
   p <- generateRepoID
   let r = RepoMetadata {pid = p, head_ = (CommitID "root"), ts = []}
   B.writeFile repoMetaPath (encode r)
   createRootDir

getPID :: IO String
getPID = do
   contents <- B.readFile repoMetaPath
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return p

setHEAD :: CommitID -> IO ()
setHEAD commitId = do
   contents <- ((decodeFileStrict repoMetaPath) :: IO (Maybe RepoMetadata))
   let (Just (RepoMetadata {pid = p, head_ = _, ts = files})) = contents
   let new = RepoMetadata {pid = p, head_ = commitId, ts = files}
   let tmpFilePath = "./.dvcs/repometadatatemp.json"
   B.writeFile tmpFilePath (encode new)
   copyFile tmpFilePath repoMetaPath

getHEAD :: IO CommitID
getHEAD = do
   contents <- B.readFile repoMetaPath
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

copyRepo :: RepoPath -> IO ()
copyRepo (LocalPath p) = do
   copyDir tempPath p
   renameDir (tempPath ++ "/" ++ (takeBaseName p)) remoteLoc

remoteCommitPath :: CommitID -> String
remoteCommitPath cid = remoteLoc ++ "/" ++ (commitPath cid)

remoteCommitMetaPath :: CommitID -> String
remoteCommitMetaPath cid = remoteLoc ++ "/" ++ (commitMetaPath cid)

getRemotePID :: IO String
getRemotePID = do
   contents <- B.readFile $ remoteRepoMetaPath
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return p

getRemoteHEAD :: IO CommitID
getRemoteHEAD = do
   contents <- B.readFile $ remoteRepoMetaPath
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return h

getRemoteTrackedSet :: IO [String]
getRemoteTrackedSet = do
   contents <- B.readFile $ remoteRepoMetaPath
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return files

getRemoteCommitChilds :: CommitID -> IO [CommitID]
getRemoteCommitChilds cid = (getCommitChildsWithPath $ remoteCommitMetaPath cid) >>=
  filterM (\f -> doesDirectoryExist (remoteLoc ++ "/" ++ objectPath ++ "/" ++ (getStr f)))

getRemoteCommitParents :: CommitID -> IO [CommitID]
getRemoteCommitParents cid = (getCommitParentsWithPath $ remoteCommitMetaPath cid) >>=
  filterM (\f -> doesDirectoryExist (remoteLoc ++ "/" ++ objectPath ++ "/" ++ (getStr f)))

-- only for testing
setRemoteHEAD :: CommitID -> IO ()
setRemoteHEAD cid = do
   contents <- ((decodeFileStrict $ remoteRepoMetaPath ) :: IO (Maybe RepoMetadata))
   let (Just (RepoMetadata {pid = p, head_ = _, ts = files})) = contents
   let new = RepoMetadata {pid = p, head_ = cid, ts = files}
   let tmpFilePath = remoteLoc ++ "/.dvcs/repometadatatemp.json"
   B.writeFile tmpFilePath (encode new)
   copyFile tmpFilePath remoteRepoMetaPath

setRemoteCommitChilds :: CommitID -> [CommitID] -> IO ()
setRemoteCommitChilds cid cids = setCommitChildsWithPath (remoteCommitMetaPath cid) cids

addRemoteCommitChilds :: CommitID -> [CommitID] -> IO ()
addRemoteCommitChilds cid cids = do
    old <- getRemoteCommitChilds cid
    setRemoteCommitChilds cid $ old ++ cids

setRemoteCommitParents :: CommitID -> [CommitID] -> IO ()
setRemoteCommitParents cid cids = setCommitParentsWithPath (remoteCommitMetaPath cid) cids

addRemoteCommitParents :: CommitID -> [CommitID] -> IO ()
addRemoteCommitParents cid cids = do
    old <- getRemoteCommitParents cid
    setRemoteCommitParents cid $ old ++ cids

getRemoteCommitDate :: CommitID -> IO UTCTime
getRemoteCommitDate cid = do
   contents <- (decodeFileStrict (remoteCommitMetaPath cid)) :: IO (Maybe CommitMeta)
   let (Just (CommitMeta {commitId = cid, message = m, date = d, childs = c, parents = p})) = contents
   return d

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

-- get the most recent common ancestor, where the local commit and the remote one
-- should have the same commitID

getUpToHead :: IO [CommitID]
getUpToHead = do
  listOfIds <- getUpToHeadRecursive [CommitID "root"]
  sorted <- sortByM (\x y -> compare <$> (getCommitDate x) <*> (getCommitDate y)) listOfIds
  return sorted

getUpToHeadRecursive :: [CommitID] -> IO [CommitID]
getUpToHeadRecursive lst = do
   when debug (putStr $ show lst ++ "\n")
   let cid = last lst
   childs <- getCommitChilds cid
   if childs == []
    then return lst
   else do
    if length childs == 1
      then do
        if elem (childs !! 0) lst
          then return lst
          else do
            result <- getUpToHeadRecursive $ lst ++ childs
            return result
      else do -- more than one child
        result <- foldM (\acc x -> getUpToHeadRecursive $ acc ++ [x]) lst childs
        return result

getUpToRootRecursive :: [CommitID] -> IO [CommitID]
getUpToRootRecursive lst = do
   let cid = last lst
   parents <- getCommitParents cid
   if parents == []
    then return lst
   else do
    if length parents == 1
      then do
        if elem (parents !! 0) lst
          then return lst
          else do
            result <- getUpToRootRecursive $ lst ++ parents
            return result
      else do
        result <- foldM (\acc x -> getUpToRootRecursive $ acc ++ [x]) lst parents
        return result

getUpToRemoteHead :: IO [CommitID]
getUpToRemoteHead = do
  listOfIds <- getUpToRemoteHeadRecursive [CommitID "root"]
  sorted <- sortByM (\x y -> compare <$> (getRemoteCommitDate x) <*> (getRemoteCommitDate y)) listOfIds
  return sorted

getUpToRemoteHeadRecursive :: [CommitID] -> IO [CommitID]
getUpToRemoteHeadRecursive lst = do
   let cid = last lst
   childs <- getRemoteCommitChilds cid
   if childs == []
    then return lst
   else do
    if length childs == 1
      then do
        if elem (childs !! 0) lst
          then return lst
          else do
            result <- getUpToRemoteHeadRecursive $ lst ++ childs
            return result
      else do -- more than one child
        result <- foldM (\acc x -> getUpToRemoteHeadRecursive $ acc ++ [x]) lst childs
        return result

getMRCA :: IO CommitID
getMRCA = do
   localHistory <- getUpToHead >>= filterM (\f -> doesDirectoryExist (objectPath ++ "/" ++ (getStr f)))
   remoteHistory <- getUpToRemoteHead >>= filterM (\f -> doesDirectoryExist (remoteLoc ++ "/" ++ objectPath ++ "/" ++ (getStr f)))
   let mrca = last $ intersect localHistory remoteHistory
   return mrca

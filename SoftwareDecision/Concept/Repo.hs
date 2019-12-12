{-# LANGUAGE DeriveGeneric #-}
module SoftwareDecision.Concept.Repo(RepoMetadata(..), createRepo, isRepo,
  getRemoteLeaf, getLocalLeaf, getHEAD, getRemoteHEAD, getPID, getRemotePID,
  RepoPath(..), getMRCA, copyRepo, getRemoteTrackedSet, getRemoteCommitChilds,
  getRemoteCommitParents, setHEAD, getUpToHead) where

import GHC.Generics
import Data.Aeson
import Data.Time
import Data.List
import System.Exit
import System.Directory (createDirectory, copyFile, doesDirectoryExist)
import Test.RandomStrings (randomString, onlyAlphaNum, randomASCII)
import qualified Data.ByteString.Lazy as B
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Concept.MetaOrganization
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Communication
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
getRemoteCommitChilds cid = getCommitChildsWithPath $ remoteCommitMetaPath cid

getRemoteCommitParents :: CommitID -> IO [CommitID]
getRemoteCommitParents cid = getCommitParentsWithPath $ remoteCommitMetaPath cid

-- only for testing
setRemoteHEAD :: CommitID -> IO ()
setRemoteHEAD cid = do
   contents <- ((decodeFileStrict $ remoteRepoMetaPath ) :: IO (Maybe RepoMetadata))
   let (Just (RepoMetadata {pid = p, head_ = _, ts = files})) = contents
   let new = RepoMetadata {pid = p, head_ = cid, ts = files}
   B.writeFile remoteRepoMetaPath (encode new)

setRemoteCommitChilds :: CommitID -> [CommitID] -> IO ()
setRemoteCommitChilds cid cids = setCommitChildsWithPath (remoteCommitMetaPath cid) cids

setRemoteCommitParents :: CommitID -> [CommitID] -> IO ()
setRemoteCommitParents cid cids = setCommitParentsWithPath (remoteCommitMetaPath cid) cids

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
getUpToHead = getUpToHeadRecursive [CommitID "root"]

getUpToHeadRecursive :: [CommitID] -> IO [CommitID]
getUpToHeadRecursive lst = do
   let cid = last lst
   childs <- getCommitChilds cid
   if childs == []
    then return lst
   else do
    lh <- getHEAD
    if foldl (&&) True ((/= lh) <$> childs)
     then do
      result <- getUpToHeadRecursive $ lst ++ (sort childs)
      return result
     else return $ lst ++ [lh]

getUpToRemoteHead :: IO [CommitID]
getUpToRemoteHead = getUpToRemoteHeadRecursive [CommitID "root"]

getUpToRemoteHeadRecursive :: [CommitID] -> IO [CommitID]
getUpToRemoteHeadRecursive lst = do
   let cid = last lst
   childs <- getRemoteCommitChilds cid
   if childs == []
    then return lst
   else do
    rh <- getRemoteHEAD
    if foldl (&&) True ((/= rh) <$> childs)
     then do
      result <- getUpToRemoteHeadRecursive $ lst ++ (sort childs)
      return result
     else return $ lst ++ [rh]

getMRCA :: IO (Maybe CommitID)
getMRCA = do
   localHistory <- getUpToHead
   remoteHistory <- getUpToRemoteHead
   let mrca = last $ intersect localHistory remoteHistory
   if mrca == (CommitID "root")
    then return Nothing
    else return $Just mrca

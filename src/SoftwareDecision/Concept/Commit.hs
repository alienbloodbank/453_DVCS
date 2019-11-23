{-# LANGUAGE DeriveGeneric #-}

module SoftwareDecision.Concept.Commit where

import Data.Time 
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import SoftwareDecision.Utility.DvcsInterface (objectPath)

-- for metadata
newtype CommitID = CommitID [Char] deriving (Generic, Show, Eq)

instance FromJSON CommitID
instance ToJSON CommitID

data CommitMeta = CommitMeta {commitId :: CommitID, message :: String, date ::UTCTime,
childs :: [CommitID], parents::[CommitID]} 
    deriving (Generic, Show)

instance FromJSON CommitMeta
instance ToJSON CommitMeta

commitPath :: CommitID -> String
commitPath (CommitID cid) = objectPath ++ "/" ++ cid

commitMetaPath :: CommitID -> String
commitMetaPath cid = (commitPath cid) ++ "/commitMeta.json" 

createCommitMeta :: CommitID -> String -> IO ()
createCommitMeta cid m = do 
    currentTime <- getCurrentTime
    let newCommit = CommitMeta {commitId = cid, message = m, date = currentTime, childs=[], parents=[]}
    B.writeFile (commitMetaPath cid) (encode newCommit)

readCommitChilds :: CommitID -> IO [CommitID]
readCommitChilds cid = do 
    contents <- B.readFile (commitMetaPath cid) 
    let (Just (CommitMeta {commitId = cid, message = m, date = d, childs = c, parents = p})) = (decode contents) :: Maybe CommitMeta
    return c

readCommitParents :: CommitID -> IO [CommitID]
readCommitParents cid = do 
    contents <- B.readFile (commitMetaPath cid) 
    let (Just (CommitMeta {commitId = cid, message = m, date = d, childs = c, parents = p})) = (decode contents) :: Maybe CommitMeta
    return p

getCommitFile :: CommitID -> String -> IO String
getCommitFile cid fp = readFile $ (commitPath cid) ++ "/" ++ fp

setCommitChilds :: CommitID -> [CommitID] -> IO()
setCommitChilds cid cids = do 
    let dest = (commitMetaPath cid)
    contents <- ((decodeFileStrict dest) :: IO (Maybe CommitMeta))
    let (Just (CommitMeta {commitId = cid, message=m, date=d, childs=c, parents=p})) = contents
    let new = CommitMeta {commitId = cid, message=m, date=d, childs=cids, parents=p}
    B.writeFile dest (encode new)

setCommitParents :: CommitID -> [CommitID] -> IO()
setCommitParents cid cids = do 
    let dest = (commitMetaPath cid)
    contents <- ((decodeFileStrict dest) :: IO (Maybe CommitMeta) )
    let (Just (CommitMeta {commitId = cid, message=m, date=d, childs=c, parents=p})) = contents
    let new = CommitMeta {commitId = cid, message=m, date=d, childs=c, parents=cids}
    B.writeFile dest (encode new)
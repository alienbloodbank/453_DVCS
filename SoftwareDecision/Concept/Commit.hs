{-# LANGUAGE DeriveGeneric #-}

module SoftwareDecision.Concept.Commit where

import System.Directory
import Data.Time 
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import SoftwareDecision.Utility.DvcsInterface (objectPath)
import Test.RandomStrings (randomString, onlyAlphaNum, randomASCII)

-- for metadata
newtype CommitID = CommitID {getStr :: String} deriving (Generic, Show, Eq)

instance FromJSON CommitID
instance ToJSON CommitID

data CommitMeta = CommitMeta {commitId :: CommitID, message :: String, date ::UTCTime,
childs :: [CommitID], parents::[CommitID]} 
    deriving (Generic, Show)

instance FromJSON CommitMeta
instance ToJSON CommitMeta

commitMetaName = "commitMeta.json" 

commitPath :: CommitID -> String
commitPath (CommitID cid) = objectPath ++ "/" ++ cid

commitMetaPath :: CommitID -> String
commitMetaPath cid = (commitPath cid) ++ "/" ++ commitMetaName

createCommitMeta :: CommitID -> String -> IO ()
createCommitMeta cid m = do 
    currentTime <- getCurrentTime
    let newCommit = CommitMeta {commitId = cid, message = m, date = currentTime, childs=[], parents=[]}
    B.writeFile (commitMetaPath cid) (encode newCommit)

generateCommitID :: IO CommitID
generateCommitID = do 
    cid <- randomString (onlyAlphaNum randomASCII) 7
    cidExist <- doesDirectoryExist (objectPath ++ "/" ++ cid)
    if cidExist
        then do 
            cid2 <- generateCommitID
            return cid2
    else
        return $ CommitID cid

createCommitDir :: String -> IO ()
createCommitDir m = do
    cid <- generateCommitID
    createDirectory (objectPath ++ "/" ++ (getStr cid))
    createCommitMeta cid m

createRootDir :: IO ()
createRootDir = do 
    createDirectory (objectPath ++ "/root")
    createCommitMeta (CommitID "root") "root of a history"

getCommitChildsWithPath :: FilePath -> IO [CommitID]
getCommitChildsWithPath fp = do 
    contents <- B.readFile fp
    let (Just (CommitMeta {commitId = cid, message = m, date = d, childs = c, parents = p})) = (decode contents) :: Maybe CommitMeta
    return c

getCommitParentsWithPath :: FilePath -> IO [CommitID]
getCommitParentsWithPath fp = do 
    contents <- B.readFile fp
    let (Just (CommitMeta {commitId = cid, message = m, date = d, childs = c, parents = p})) = (decode contents) :: Maybe CommitMeta
    return p

getCommitChilds :: CommitID -> IO [CommitID]
getCommitChilds cid = getCommitChildsWithPath (commitMetaPath cid)

getCommitParents :: CommitID -> IO [CommitID]
getCommitParents cid = getCommitParentsWithPath (commitMetaPath cid)

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
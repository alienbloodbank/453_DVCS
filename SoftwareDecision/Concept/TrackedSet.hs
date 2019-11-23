{-# LANGUAGE DeriveGeneric #-}

module SoftwareDecision.Concept.TrackedSet (addFile, removeFile, getTracketSet) where

--import SoftwareDecision.Concept.Repo
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.List as List

data RepoMetadata = 
    RepoMetadata {
      pid :: String
     ,head_ :: String
     ,ts :: [String]
    } deriving (Generic, Show)

instance FromJSON RepoMetadata
instance ToJSON RepoMetadata

addFile :: String -> IO ()
addFile fileName = do
   putStrLn fileName
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   let new = RepoMetadata {pid = p, head_ = h, ts = List.nub $ fileName:files}
   B.writeFile "./.dvcs/repometadatatemp.json" (encode new)

removeFile :: String -> IO ()
removeFile fileName = do
   putStrLn fileName
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   let new = RepoMetadata {pid = p, head_ = h, ts = List.filter (\x -> x /= fileName) files}
   B.writeFile "./.dvcs/repometadatatemp.json" (encode new)

getTracketSet :: IO [String]
getTracketSet = do
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return files

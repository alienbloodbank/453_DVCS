{-# LANGUAGE DeriveGeneric #-}
module SoftwareDecision.Concept.Repo(RepoMetadata(..), createRepo) where

import GHC.Generics
import Data.Aeson

import System.Directory (createDirectory, copyFile)
import Test.RandomStrings (randomString, randomChar)
import qualified Data.ByteString.Lazy as B

data RepoMetadata = RepoMetadata {
      pid :: String
     ,head_ :: String
     ,ts :: [String]
    } deriving (Generic, Show)

instance FromJSON RepoMetadata
instance ToJSON RepoMetadata

createRepo :: IO ()
createRepo = do
   createDirectory "./.dvcs"
   pid <- randomString randomChar 20
   writeFile "./.dvcs/repometadata.json" ("{\"pid\":\"" ++ pid ++ "\",\"ts\":[], \"head_\":\"root\"}")
   writeFile "./.dvcs/repometadatatemp.json" "" 

getPID :: IO String
getPID = do
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return p

setHEAD :: String -> IO ()
setHEAD commitId = do
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = _, ts = files})) = (decode contents) :: Maybe RepoMetadata
   let new = RepoMetadata {pid = p, head_ = commitId, ts = files}
   B.writeFile "./.dvcs/repometadatatemp.json" (encode new)
   copyFile "./.dvcs/repometadatatemp.json" "./.dvcs/repometadata.json"

getHEAD :: IO String
getHEAD = do
   contents <- B.readFile "./.dvcs/repometadata.json"
   let (Just (RepoMetadata {pid = p, head_ = h, ts = files})) = (decode contents) :: Maybe RepoMetadata
   return h 

{-# LANGUAGE DeriveGeneric #-}
module SoftwareDecision.Concept.Repo(RepoMetadata(..)) where

import GHC.Generics

data RepoMetadata = RepoMetadata {
      pid :: String
     ,head_ :: String
     ,ts :: [String]
    } deriving (Generic, Show)




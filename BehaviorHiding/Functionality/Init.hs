module BehaviorHiding.Functionality.Init(performInit) where

import System.Directory (getCurrentDirectory)
import SoftwareDecision.Concept.Repo

performInit :: IO String
performInit = do
   doesRepoAlreadyExist <- isRepo
   cd <- getCurrentDirectory
   if doesRepoAlreadyExist then return $ "Reinitialized existing dvcs repository in " ++ cd
   else do
      createRepo
      return $ "Initialized repository in " ++ cd

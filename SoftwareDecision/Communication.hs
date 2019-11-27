module SoftwareDecision.Communication (uploadRemoteDir, downloadRemoteDir, doesRemoteDirExist) where

import System.Process

downloadRemoteDir :: String -> IO ()
downloadRemoteDir remotePath = do
   _ <- readProcess "scp" ["-r", remotePath, "."] ""
   return () 

uploadRemoteDir :: String -> IO ()
uploadRemoteDir remotePath = do
   _ <- system $ "scp -r * " ++ remotePath
   return ()

doesRemoteDirExist :: String -> String -> IO Bool
doesRemoteDirExist hostname path = do
   let createProcess = shell $ "if ssh " ++ hostname ++ " '[ -d " ++ path ++ " ]'; then echo \"Y\"; else echo \"N\"; fi"
   out <- readCreateProcess createProcess ""
   return ((unwords . words $ out) == "Y")

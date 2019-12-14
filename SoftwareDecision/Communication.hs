module SoftwareDecision.Communication (uploadRemoteDir, downloadRemoteDir, doesRemoteDirExist) where

import System.Process

downloadRemoteDir :: String -> IO ()
downloadRemoteDir remotePath = do
   callCommand $ "rsync -a -essh " ++ remotePath ++ " ."

uploadRemoteDir :: String -> IO ()
uploadRemoteDir remotePath = do
   callCommand $ "rsync -a -essh . " ++ remotePath

doesRemoteDirExist :: String -> String -> IO Bool
doesRemoteDirExist hostname path = do
   let createProcess = shell $ "if ssh " ++ hostname ++ " '[ -d " ++ path ++ " ]'; then echo \"Y\"; else echo \"N\"; fi"
   out <- readCreateProcess createProcess ""
   return ((unwords . words $ out) == "Y")


module SoftwareDecision.Utility.DvcsInterface(insertDirs, dvcsName, objectPath, 
    metaPath, historyPath, copyDir, renameDir) where

import System.Directory
import System.Process
import System.Exit

dvcsName = "dvcs"
objectRelativePath = "snapshot"
objectPath = "." ++ dvcsName ++ "/" ++ objectRelativePath

metaRelativePath = "info"
metaPath = "." ++ dvcsName ++ "/" ++ metaRelativePath

-- we can also store the linked list in a separate file 
historyPath = metaPath ++ "/history"

-- all the reading

findDir :: FilePath -> IO String
findDir fp = head <$> lines <$> (readProcess "find" [".dvcs", "-name", fp, "-print"] "")

readLines :: FilePath -> IO [String]
readLines = (fmap lines). readFile

readFirstLine :: FilePath -> IO String
readFirstLine fp = fmap head (readLines fp)

-- not just read

-- there is a function called copyFile in the module System.Directory
-- it can be used for create commit 

copyDir ::  FilePath -> FilePath -> IO ExitCode
copyDir dest src = system $ "cp -r " ++ src ++ " " ++ dest

insertDirs :: [String] -> String -> IO()
insertDirs srcs dest = sequence_ $ fmap (copyDir dest) srcs

renameDir :: String -> String -> IO ()
renameDir old new = do 
    putStrLn $ "mv " ++ old ++ " " ++ new
    system $ "mv " ++ old ++ " " ++ new
    return ()
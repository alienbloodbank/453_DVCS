
module SoftwareDecision.Utility.DvcsInterface(insertDirs, dvcsName, objectPath, 
    metaPath, historyPath) where

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

-- pidPath =  metaPath ++ "/pid"
-- headPath = metaPath ++ "/head"
-- tsPath = metaPath ++ "/ts"

-- all the reading

readLines :: FilePath -> IO [String]
readLines = (fmap lines). readFile

readFirstLine :: FilePath -> IO String
readFirstLine fp = fmap head (readLines fp)

-- readPid = readFile pidPath
-- readHead = readFile headPath
-- readTs = fmap fromList $ readLines tsPath

-- not just read

copyDir ::  FilePath -> FilePath -> IO ExitCode
copyDir dest src = system $ "cp -r " ++ src ++ " " ++ dest

insertDirs :: [String] -> String -> IO()
insertDirs srcs dest = sequence_ $ fmap (copyDir dest) srcs

-- setCommitChildren :: CommitID -> [CommitID] -> IO()
-- setCommitChildren cid ids = writeFile dest $ unlines $ getId <$> ids
--     where dest = childrenPath cid

-- setCommitParents :: CommitID -> [CommitID] -> IO()
-- setCommitParents cid ids = writeFile dest $ unlines $ getId <$> ids
--     where dest = parentsPath cid
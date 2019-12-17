
module SoftwareDecision.Utility.DvcsInterface where

import System.Directory
import System.Process
import System.Exit
import Control.Monad
import Control.Monad.ListM

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
    system $ "mv " ++ old ++ " " ++ new
    return ()

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive filePath = do
    files <- listDirectory filePath
    let filteredFiles = filter (\f -> not . or $ [f == ".git", f == ".dvcs"]) files
    filesOnly <- foldM (\acc x -> do
                               isFile <- doesFileExist $ filePath ++ "/" ++ x
                               if isFile then return (x:acc)
                               else do
                                   files_ <- listDirectoryRecursive $ filePath ++ "/" ++ x
                                   let withPath = map (\f -> x ++ "/" ++ f) files_
                                   return $ withPath ++ acc) [] filteredFiles
    return filesOnly

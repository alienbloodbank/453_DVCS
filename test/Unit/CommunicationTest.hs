import Test.HUnit
import System.Process
import System.Posix.User
import System.Directory (doesDirectoryExist, setCurrentDirectory, doesFileExist, getHomeDirectory, createDirectory)
import BehaviorHiding.Functionality
import SoftwareDecision.Utility.DvcsInterface (copyDir)
import SoftwareDecision.Communication
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.Repo

import Data.Set as Set

main = do
    -- build the test senario
    performInit
    homeDir <- getHomeDirectory
    let remote_path = "~/test_repo"
    remoteExist <- doesDirectoryExist remote_path
    if remoteExist
        then print "the remote test repo already exists"
        else createDirectory $ homeDir ++ "/test_repo"
    copyDir remote_path ".dvcs"
    
    username <- getLoginName

    -- begin testing

    -- downloadRemoteDir
    downloadRemoteDir $ username ++ "@127.0.0.1:~/test_repo/.dvcs"
    doesExist <- doesDirectoryExist ".dvcs" 
    let test1 = "test1" ~: "downloadRemoteDir" ~: True ~=? doesExist

    -- uploadRemoteDir
    _ <- system "touch .dvcs/foo.json"
    setCurrentDirectory ".dvcs"
    uploadRemoteDir $ username ++ "@127.0.0.1:~/test_repo/.dvcs"
    setCurrentDirectory ".."
    doesExist <- doesFileExist $ homeDir ++ "/test_repo/.dvcs/foo.json"
    let test2 = "test2" ~: "uploadRemoteDir" ~: True ~=? doesExist
    
    -- doesRemoteDirExist
    doesExist <- doesRemoteDirExist (username ++ "@127.0.0.1") "~/test_repo/.dvcs"
    let test3 = "test3" ~: "doesRemoteDirExist" ~: True ~=? doesExist
 
    let tests = test [test1, test2, test3]
    runTestTT tests


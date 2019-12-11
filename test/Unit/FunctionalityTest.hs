import Test.HUnit
import System.Process
import System.Posix.User
import System.Directory (removeDirectoryRecursive, removeFile, createDirectory, doesDirectoryExist, setCurrentDirectory, doesFileExist, getHomeDirectory, getCurrentDirectory)
import BehaviorHiding.Functionality
import SoftwareDecision.Concept.Repo
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Utility.DvcsInterface (copyDir)

main = do
    -- build the test senario
    homeDir <- getHomeDirectory
    username <- getLoginName
    let remote_path = homeDir ++ "/test_repo"
    createDirectory remote_path 

    -- begin testing

    -- performInit
    msg <- performInit
    cd <- getCurrentDirectory
    let test1_1 = "test1_1" ~: "performInit" ~: "Initialized repository in " ++ cd ~=? msg
    msg <- performInit
    let test1_2 = "test1_2" ~: "performInit" ~: ("Reinitialized existing dvcs repository in " ++ cd) ~=? msg

    -- performClone
    copyDir remote_path ".dvcs"
    msg <- performClone remote_path
    let test2_1 = "test2_1" ~: "performClone" ~: "Cloned local repository" ~=? msg
    msg <- performClone $ username ++ "@127.0.0.1:"++ remote_path
    let test2_2 = "test2_2" ~: "performClone" ~: "Cloned remote repository" ~=? msg
    removeDirectoryRecursive "test_repo"

    -- performAdd
    msg <- performAdd "foo.txt"
    let test3_1 = "test3_1" ~: "performAdd" ~: "fatal: File does not exist in current directory" ~=? msg
    _ <- system "touch foo.txt"
    msg <- performAdd "foo.txt"
    let test3_2 = "test3_2" ~: "performAdd" ~: "File added" ~=? msg 
    removeFile "foo.txt"
    msg <- performAdd "foo.txt"
    let test3_3 = "test3_3" ~: "performAdd" ~: "File removed as its not in current directory" ~=? msg
    
    -- performRemove
    msg <- performRemove "foo.txt"
    let test4_1 = "test4_1" ~: "performRemove" ~: "Error: File not being tracked. Nothing to remove" ~=? msg
    _ <- system "touch foo.txt"
    _ <- performAdd "foo.txt"  
    msg <- performRemove "foo.txt"
    removeFile "foo.txt"
    let test4_2 = "test4_2" ~: "performRemove" ~: "File removed" ~=? msg
     
    -- performStatus
    msg <- performStatus 
    let test5 = "test5" ~: "performStatus" ~: "Repository status" ~=? msg

    -- performCommit & performHeads & performLog
    msg <- performHeads
    let test6_1 = "test6_1" ~: "performHeads" ~: "fatal: no commits in current repository." ~=? msg
    msg <- performLog
    let test6_2 = "test6_2" ~: "performLog" ~: "fatal: no commits in current repository." ~=? msg
    _ <- system "echo 'Hello, world.' >foo.txt"
    _ <- performAdd "foo.txt"
    msg <- performCommit "m1"
    let test6_3 = "test6_3" ~: "performCommit" ~: "Committed" ~=? msg 
    msg <- performHeads
    let test6_4 = "test6_4" ~: "performHeads" ~: "Heads shown" ~=? msg
    msg <- performLog
    let test6_5 = "test6_5" ~: "performLog" ~: "Commit history" ~=? msg
    
    -- performCheckout
    msg <- performCheckout "xxxxxxx"
    let test7_1 = "test7_1" ~: "performCheckout" ~: "fatal: invalid commit id." ~=? msg
    rev <- getHEAD
    msg <- performCheckout (getStr rev)
    let test7_2 = "test7_2" ~: "performCheckout" ~: ("Head successfully changed to " ++ (getStr rev)) ~=? msg

    -- performCat
    rev <- getHEAD
    msg <- performCat (getStr rev) "foo.txt"
    let test8 = "test8" ~: "performCat" ~: "Hello, world.\n" ~=? msg

    removeFile "foo.txt" 

    let tests = test [test1_1, test1_2, test2_1, test2_2, test3_1, test3_2, test3_3, test4_1, test4_2, test5, 
                test6_1, test6_2, test6_3, test6_4, test6_5, test7_1, test7_2, test8]
    runTestTT tests


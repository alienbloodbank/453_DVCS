import Test.HUnit
import System.Process
import BehaviorHiding.Functionality
import SoftwareDecision.Concept.TrackedSet as TS
import SoftwareDecision.Utility.DvcsInterface
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.Repo

import Data.Set as Set

main = do
    -- build the test senario
    performInit

    -- begin testing

    -- getTrackedSet
    trackedSet <- TS.getTrackedSet
    let test1 = "test1" ~: "getTrackedSet" ~: [] ~=? trackedSet
    
    -- addFile
    TS.addFile "bar.txt"
    TS.addFile "foo.txt"
    TS.addFile "lol.txt"
    trackedSet <- TS.getTrackedSet
    let test2 = "test2" ~: "addFile" ~: Set.fromList ["bar.txt", "foo.txt", "lol.txt"] ~=? Set.fromList trackedSet

    -- removeFile
    TS.removeFile "foo.txt"
    trackedSet <- TS.getTrackedSet
    let test3 = "test3" ~: "removeFile" ~: Set.fromList ["bar.txt", "lol.txt"] ~=? Set.fromList trackedSet 

    -- cleanTrackedSet
    _ <- system "touch lol.txt"
    TS.cleanTrackedSet
    trackedSet <- TS.getTrackedSet
    _ <- system "rm lol.txt"
    let test4 = "test4" ~: "cleanTrackedSet" ~: Set.fromList ["lol.txt"] ~=? Set.fromList trackedSet

    let tests = test [test1, test2, test3, test4]
    runTestTT tests

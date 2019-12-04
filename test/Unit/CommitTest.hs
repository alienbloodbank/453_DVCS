import Test.HUnit
import BehaviorHiding.Functionality (performInit)
import SoftwareDecision.Concept.Commit (CommitID, getCommitChilds, getCommitID, createCommitDir, 
    getCommitParents, setCommitChilds, addCommitChilds, getCommitMessage)

main = do 
    performInit
    -- test createCommitDir, getCommitChilds, getCommitParents
    let message1 = "test createCommitDir"
    cid <- createCommitDir message1
    cid2 <- getCommitID cid
    let test1_1 = TestCase (assertEqual "wrong id" cid cid2)
    actual_message <- (getCommitMessage cid2)
    let test1_2 = TestCase (assertEqual "wrong message" message1 actual_message)
    actual_childs <- (getCommitChilds cid2)
    let test1_3 = TestCase (assertEqual "wrong childs" ([]::[CommitID]) actual_childs)
    actual_parents <- (getCommitParents cid2)
    let test1_4 = TestCase (assertEqual "wrong parents" ([]::[CommitID]) actual_parents)

    -- test setCommitChilds
    let message2 = "test setCommitChilds"
    cid3 <- createCommitDir message2
    setCommitChilds cid2 [cid3]
    actual_childs <- (getCommitChilds cid2)
    let test2_1 = TestCase (assertEqual "wrong set childs" [cid3] actual_childs)

    setCommitChilds cid2 ([]::[CommitID])
    actual_childs <- (getCommitChilds cid2)
    let test2_2 = TestCase (assertEqual "wrong set childs 2" ([]::[CommitID]) actual_childs)

    -- test addCommitChilds
    let message3 = "test addCommitChilds"

    addCommitChilds cid2 ([]::[CommitID])
    actual_childs <- (getCommitChilds cid2)
    let test3_1 = TestCase (assertEqual "wrong add childs" ([]::[CommitID]) actual_childs)

    cid4 <- createCommitDir message3
    addCommitChilds cid2 [cid4]
    actual_childs <- (getCommitChilds cid2)
    let test3_2 = TestCase (assertEqual "wrong add childs 2" [cid4] actual_childs)

    addCommitChilds cid3 [cid2, cid4]
    actual_childs <- (getCommitChilds cid3)
    let test3_3 = TestCase (assertEqual "wrong add childs 3" [cid2, cid4] actual_childs)

    let tests = TestList[TestLabel "test1_1" test1_1, TestLabel "test1_2" test1_2,
                     TestLabel "test1_3" test1_3, TestLabel "test1_4" test1_4,
                     TestLabel "test2_1" test2_1, TestLabel "test3_1" test3_1,
                     TestLabel "test3_2" test3_2, TestLabel "test3_3" test3_3]

    runTestTT tests
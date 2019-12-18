import Test.HUnit
import System.Directory (createDirectory, doesDirectoryExist, removeDirectoryRecursive, getHomeDirectory)
import System.Process

import BehaviorHiding.Functionality.Init
import SoftwareDecision.Concept.TrackedSet
import SoftwareDecision.Utility.DvcsInterface (copyDir)
import SoftwareDecision.Concept.Commit
import SoftwareDecision.Concept.Repo

main = do
    -- build the test senario
    performInit
    addFile "dvcs.hs"
    homeDir <- getHomeDirectory
    cid1 <- createCommitDir "mrca"
    addCommitChilds (CommitID "root") [cid1]
    setCommitParents cid1 [CommitID "root"]
    cid2 <- createCommitDir "branch 1"
    cid3 <- createCommitDir "branch 2"
    addCommitChilds cid1 [cid2]
    let remote_path = "~/test_repo"
    remoteExist <- doesDirectoryExist remote_path
    if remoteExist
        then print "the remote test repo already exists"
        else createDirectory $ homeDir ++ "/test_repo"
    setHEAD cid2
    copyDir remote_path ".dvcs"

    -- setCommitChilds cid1 ([]::[CommitID])
    addCommitChilds cid1 [cid3]
    cid4 <- createCommitDir "branch 2 second"
    addCommitChilds cid3 [cid4]
    copyRepo $ LocalPath remote_path

    -- begin testing

    -- test getUpToHead
    cid5 <- createCommitDir "branch3"
    cid6 <- createCommitDir "merge 1"
    cid7 <- createCommitDir "merge 2"
    setCommitChilds cid1 [cid2, cid3, cid5]
    setCommitChilds cid5 [cid6]
    setCommitChilds cid2 [cid6]
    setCommitChilds cid6 [cid7]
    setCommitChilds cid4 [cid7]
    setHEAD cid7
    upToHead <- getUpToHead
    let test0_1 = TestCase (assertEqual "wrong upToHead" [CommitID "root", cid1, cid2, cid3, cid4, cid5, cid6, cid7] upToHead)

    setCommitChilds cid1 ([cid3])
    setHEAD cid2
    -- test getRemoteLeaf
    remote_leaf <- getRemoteLeaf
    let test1_1 = TestCase (assertEqual "wrong remote leaf" cid2 remote_leaf)

    -- test getRemotePID
    remote_pid <- getRemotePID
    local_pid <- getPID
    let test2_1 = TestCase (assertEqual "wrong remote Project ID" local_pid remote_pid)

    -- test getRemoteCommitChilds
    remote_childs <- getRemoteCommitChilds (CommitID "root")
    let test3_1 = TestCase (assertEqual "wrong remote childs" [cid1] remote_childs)

    -- test getRemoteCommitParents
    remote_parents <- getRemoteCommitParents cid1
    let test4_1 = TestCase (assertEqual "wrong remote parents" [CommitID "root"] remote_parents)

    -- test getMRCA
    maybe_mrca <- getMRCA
    let mrca = maybe_mrca
    let test5_1 = TestCase (assertEqual "wrong mrca" cid1 mrca)

    -- test getRemoteTrackedSet
    remote_ts <- getRemoteTrackedSet
    let test6_1 = TestCase (assertEqual "wrong remote TrackedSet" ["dvcs.hs"] remote_ts)

    -- test getRemoteHEAD
    remote_head <- getRemoteHEAD
    let test7_1 = TestCase (assertEqual "wrong remote head" cid2 remote_head)

    let tests = TestList[TestLabel "test0_1" test0_1, TestLabel "test1_1" test1_1,
                         TestLabel "test2_1" test2_1, TestLabel "test3_1" test3_1,
                         TestLabel "test4_1" test4_1, TestLabel "test5_1" test5_1,
                         TestLabel "test6_1" test6_1, TestLabel "test7_1" test7_1]
    runTestTT tests
    system "rm -r ~/test_repo"
    system "rm -r .dvcs"

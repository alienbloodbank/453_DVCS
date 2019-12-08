import Test.HUnit
import BehaviorHiding.Functionality
import BehaviorHiding.UserInteraction

main = do
    -- begin testing

    -- parse
    msg <- parse []
    let test1_1 = "test1_1" ~: "parse" ~: "usage: dvcs <command> [<args>]. See 'dvcs help'" ~=? msg    
    msg <- parse ["foo"]
    let test1_2 = "test1_2" ~: "parse" ~: "dvcs: foo is not a dvcs command. See 'dvcs help’" ~=? msg        

    -- postProcess
    msg <- parse ["help"]
    let test2_1 = "test2_1" ~: "postProcess: help" ~: ("usage:\n\n" ++
          "- ./dvcs init\n" ++
          "- ./dvcs add <file> # Only adds files!\n" ++
          "- ./dvcs remove <file>\n" ++
          "- ./dvcs status\n" ++
          "- ./dvcs clone <path>\n" ++
          "- ./dvcs commit <message>\n" ++
          "- ./dvcs heads\n" ++
          "- ./dvcs log\n" ++
          "- ./dvcs diff <commit_id1> <commit_id2>\n" ++
          "- ./dvcs cat <commit_id> <file>\n" ++
          "- ./dvcs checkout <commit_id>\n") ~=? msg
    msg <- parse ["clone"]
    let test2_2 = "test2_2" ~: "postProcess: clone" ~: "fatal: You must specify a repository to clone." ~=? msg
    msg <- parse ["add"]
    let test2_3 = "test2_3" ~: "postProcess: add" ~: "Nothing specified, nothing added." ~=? msg
    msg <- parse ["remove"]
    let test2_4 = "test2_4" ~: "postProcess: remove" ~: "Nothing specified, nothing removed." ~=? msg
    msg <- parse ["diff"]
    let test2_5 = "test2_5" ~: "postProcess: diff" ~: "No commits provided" ~=? msg
    msg <- parse ["diff", "kjbhjfk"]
    let test2_6 = "test2_6" ~: "postProcess: diff" ~: "No second commit to diff against" ~=? msg
    msg <- parse ["checkout"]
    let test2_7 = "test2_7" ~: "postProcess: checkout" ~: "No commit id provided to checkout" ~=? msg
    msg <- parse ["commit"]
    let test2_8 = "test2_8" ~: "postProcess: commit" ~: "No commit message provided" ~=? msg
    msg <- parse ["cat"]
    let test2_9 = "test2_9" ~: "postProcess: cat" ~: "No filename given, no commit id given" ~=? msg
    msg <- parse ["cat", ""]
    let test2_10 = "test2_10" ~: "postProcess: cat" ~: "No filename given" ~=? msg

    let tests = test [test1_1, test1_2, test2_1, test2_2, test2_3, test2_4, test2_5, test2_6, test2_7, test2_8, test2_9, test2_10]
    runTestTT tests


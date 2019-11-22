module UserInteraction(parse) where

-- User Interaction module uses Functionality module
import Functionality

validCommands :: [String]
validCommands = ["init", "clone", "add", "remove", "status", "heads", "diff", "cat", "checkout", "commit", "log", "merge", "pull", "push"]

-- Ignores extra arguments everywhere
process :: String -> [String] -> String
process "init" args = performInit
process "clone" args
  | args == [] = "fatal: You must specify a repository to clone."
  | otherwise = performClone (args !! 0)
process "add" args
  | args == [] = "Nothing specified, nothing added."
  | otherwise = performAdd args
process "remove" args
  | args == [] = "usage: dvcs rm <file>..."
  | otherwise = performRemove args
process "status" args = performStatus
process "heads" args = performHeads
process "diff" args
  | args == [] = "No revisions provided"
  | (length args) == 1 = "No second revision to diff against"
  | otherwise = performDiff (args !! 0) (args !! 1)
process "log" args = performLog
process "checkout" args
  | args == [] = "No revision provided to checkout"
  | otherwise = performCheckout (args !! 0)
process "commit" args
  | args == [] = "No commit message provided"
  | otherwise = performCommit (args !! 0)
process "cat" args
  | (length args) == 0 || (length args) == 1 = "No filename given, no commit_id given"
  | otherwise = performCat (args !! 0) (args !! 1)
process "pull" args
  | args == [] = "There is no tracking information for the current branch."
  | otherwise = performPull (args !! 0)
process "push" args
  | args == [] = "fatal: No configured push destination."
  | otherwise = performPush (args !! 0) 

parse :: [String] -> String
parse args = case args of [] -> "usage: dvcs <command> [<args>]"
                          (command: res) -> if (command `elem` validCommands) 
                                            then (process command res) 
                                            else ("dvcs: " ++ command ++ " is not a dvcs command. See 'dvcs --help’")


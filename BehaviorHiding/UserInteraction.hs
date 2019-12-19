module BehaviorHiding.UserInteraction(parse) where

-- User Interaction module uses Functionality module
import BehaviorHiding.Functionality.Add
import BehaviorHiding.Functionality.Cat
import BehaviorHiding.Functionality.Checkout
import BehaviorHiding.Functionality.Clone
import BehaviorHiding.Functionality.Commit
import BehaviorHiding.Functionality.Diff
import BehaviorHiding.Functionality.Heads
import BehaviorHiding.Functionality.Init
import BehaviorHiding.Functionality.Log
import BehaviorHiding.Functionality.Pull
import BehaviorHiding.Functionality.Push
import BehaviorHiding.Functionality.Remove
import BehaviorHiding.Functionality.Status

import Control.Monad

validCommands :: [String]
validCommands = ["help", "init", "clone", "add", "remove", "status", "heads", "diff", "cat", "checkout", "commit", "log", "merge", "pull", "push"]

-- Ignores extra arguments everywhere
postProcess :: String -> [String] -> IO String
postProcess "help" _ = do
   return $ "usage:\n\n" ++
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
          "- ./dvcs checkout [<commit_id>]\n" ++
          "- ./dvcs pull <path>\n" ++
          "- ./dvcs push <path>\n"
postProcess "init" args = performInit >>= return
postProcess "clone" args = do
   if args == [] then return "fatal: You must specify a repository to clone."
   else performClone (args !! 0) >>= return
postProcess "add" args = do
   if args == [] then return "Nothing specified, nothing added."
   else performAdd (args !! 0) >>= return
postProcess "remove" args = do
   if args == [] then return "Nothing specified, nothing removed."
   else performRemove (args !! 0) >>= return
postProcess "status" _ = performStatus >>= return
postProcess "heads" _ = performHeads >>= return
postProcess "diff" args = do
   if args == [] then return "No commits provided"
   else if (length args) == 1 then return "No second commit to diff against"
   else performDiff (args !! 0) (args !! 1) >>= return
postProcess "log" args = performLog >>= return
postProcess "checkout" args = do
   if args == [] then performCheckout "LEAF" >>= return
   else performCheckout (args !! 0) >>= return
postProcess "commit" args = do
   if args == [] then return "No commit message provided"
   else performCommit (args !! 0) >>= return
postProcess "cat" args = do
   if (length args) == 0 then return "No filename given, no commit id given"
   else if (length args) == 1 then return "No filename given"
   else performCat (args !! 0) (args !! 1) >>= return
postProcess "pull" args = do
   if args == [] then return "There is no tracking information for the current branch."
   else performPull (args !! 0) >>= return
postProcess "push" args = do
   if args == [] then return "fatal: No configured push destination."
   else performPush (args !! 0) >>= return

parse :: [String] -> IO String
parse [] = do return "usage: dvcs <command> [<args>]. See 'dvcs help'"
parse (command: res) = do
    if (command `elem` validCommands) then postProcess command res >>= return
    else return ("dvcs: " ++ command ++ " is not a dvcs command. See 'dvcs help’")

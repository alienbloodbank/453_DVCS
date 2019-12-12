module BehaviorHiding.UserInteraction(parse) where

-- User Interaction module uses Functionality module
import BehaviorHiding.Functionality
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
          "- ./dvcs checkout <commit_id>\n"
postProcess "init" args = performInit >>= \msg -> return msg
postProcess "clone" args = do
   if args == [] then return "fatal: You must specify a repository to clone."
   else performClone (args !! 0) >>= \msg -> return msg
postProcess "add" args = do
   if args == [] then return "Nothing specified, nothing added."
   else performAdd (args !! 0) >>= \msg -> return msg
postProcess "remove" args = do
   if args == [] then return "Nothing specified, nothing removed."
   else performRemove (args !! 0) >>= \msg -> return msg
postProcess "status" _ = performStatus >>= \msg -> return msg
postProcess "heads" _ = performHeads >>= \msg -> return msg
postProcess "diff" args = do
   if args == [] then return "No commits provided"
   else if (length args) == 1 then return "No second commit to diff against"
   else do performDiff (args !! 0) (args !! 1) >>= \msg -> return msg
postProcess "log" args = performLog >>= \msg -> return msg
postProcess "checkout" args = do
   if args == [] then return "No commit id provided to checkout"
   else performCheckout (args !! 0) >>= \msg -> return msg
postProcess "commit" args = do
   if args == [] then return "No commit message provided"
   else performCommit (args !! 0) >>= \msg -> return msg
postProcess "cat" args = do
   if (length args) == 0 then return "No filename given, no commit id given"
   else if (length args) == 1 then return "No filename given"
   else performCat (args !! 0) (args !! 1) >>= \msg -> return msg
postProcess "pull" args = do
   if args == [] then return "There is no tracking information for the current branch."
   else performPull (args !! 0) >>= \msg -> return msg
postProcess "push" args = do
   if args == [] then return "fatal: No configured push destination."
   else performPush (args !! 0) >>= \msg -> return msg

parse :: [String] -> IO String
parse [] = do return "usage: dvcs <command> [<args>]. See 'dvcs help'"
parse (command: res) = do
    if (command `elem` validCommands) then postProcess command res >>= \msg -> return msg
    else return ("dvcs: " ++ command ++ " is not a dvcs command. See 'dvcs help’")

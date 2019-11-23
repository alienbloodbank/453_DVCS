module BehaviorHiding.UserInteraction(parse) where

-- User Interaction module uses Functionality module
import BehaviorHiding.Functionality

validCommands :: [String]
validCommands = ["init", "clone", "add", "remove", "status", "heads", "diff", "cat", "checkout", "commit", "log", "merge", "pull", "push"]

-- Ignores extra arguments everywhere
process :: String -> [String] -> IO String
process "init" args = do
   msg <- performInit
   return msg
process "clone" args = do
   if args == [] then return "fatal: You must specify a repository to clone."
   else do
     msg <- performClone (args !! 0) 
     return msg
process "add" args = do
   if args == [] then return "Nothing specified, nothing added."
   else do 
     msg <- performAdd (args !! 0)
     return msg
process "remove" args = do
   if args == [] then return "usage: dvcs rm <file>..."
   else do 
     msg <- performRemove (args !! 0)
     return msg
process "status" args = do 
   msg <- performStatus
   return msg
process "heads" args = do
   msg <- performHeads
   return msg
process "diff" args = do
   if args == [] then return "No revisions provided"
   else if (length args) == 1 then return "No second revision to diff against"
   else do
     msg <- performDiff (args !! 0) (args !! 1)
     return msg
process "log" args = do
   msg <- performLog
   return msg
process "checkout" args = do
   if args == [] then return "No revision provided to checkout"
   else do 
     msg <- performCheckout (args !! 0)
     return msg
process "commit" args = do
   if args == [] then return "No commit message provided"
   else do 
     msg <- performCommit (args !! 0)
     return msg
process "cat" args = do
   if (length args) == 0 || (length args) == 1 then return "No filename given, no commit_id given"
   else do 
     msg <- performCat (args !! 0) (args !! 1)
     return msg
process "pull" args = do
   if args == [] then return "There is no tracking information for the current branch."
   else do 
     msg <- performPull (args !! 0)
     return msg
process "push" args = do
   if args == [] then return "fatal: No configured push destination."
   else do 
     msg <- performPush (args !! 0) 
     return msg

parse :: [String] -> IO String
parse [] = do return "usage: dvcs <command> [<args>]"
parse (command: res) = do
    if (command `elem` validCommands)
    then do 
       msg <- (process command res)
       return msg
       else return ("dvcs: " ++ command ++ " is not a dvcs command. See 'dvcs --help’")

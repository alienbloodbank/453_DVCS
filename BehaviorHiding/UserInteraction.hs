module BehaviorHiding.UserInteraction(parse) where

-- User Interaction module uses Functionality module
import BehaviorHiding.Functionality

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
postProcess "init" args = do
   msg <- performInit
   return msg
postProcess "clone" args = do
   if args == [] then return "fatal: You must specify a repository to clone."
   else do
     msg <- performClone (args !! 0) 
     return msg
postProcess "add" args = do
   if args == [] then return "Nothing specified, nothing added."
   else do 
     msg <- performAdd (args !! 0)
     return msg
postProcess "remove" args = do
   if args == [] then return "Nothing specified, nothing removed."
   else do 
     msg <- performRemove (args !! 0)
     return msg
postProcess "status" _ = do 
   msg <- performStatus
   return msg
postProcess "heads" _ = do
   msg <- performHeads
   return msg
postProcess "diff" args = do
   if args == [] then return "No commits provided"
   else if (length args) == 1 then return "No second commit to diff against"
   else do
     msg <- performDiff (args !! 0) (args !! 1)
     return msg
postProcess "log" args = do
   msg <- performLog
   return msg
postProcess "checkout" args = do
   if args == [] then return "No commit id provided to checkout"
   else do 
     msg <- performCheckout (args !! 0)
     return msg
postProcess "commit" args = do
   if args == [] then return "No commit message provided"
   else do 
     msg <- performCommit (args !! 0)
     return msg
postProcess "cat" args = do
   if (length args) == 0 then return "No filename given, no commit id given"
   else if (length args) == 1 then return "No filename given"
   else do 
     msg <- performCat (args !! 0) (args !! 1)
     return msg
postProcess "pull" args = do
   if args == [] then return "There is no tracking information for the current branch."
   else do 
     msg <- performPull (args !! 0)
     return msg
postProcess "push" args = do
   if args == [] then return "fatal: No configured push destination."
   else do 
     msg <- performPush (args !! 0) 
     return msg

parse :: [String] -> IO String
parse [] = do return "usage: dvcs <command> [<args>]. See 'dvcs help'"
parse (command: res) = do
    if (command `elem` validCommands)
    then do 
       msg <- (postProcess command res)
       return msg
       else return ("dvcs: " ++ command ++ " is not a dvcs command. See 'dvcs help’")

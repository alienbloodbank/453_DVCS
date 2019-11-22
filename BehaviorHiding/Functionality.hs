module BehaviorHiding.Functionality(performInit, 
performClone, 
performAdd, 
performRemove, 
performStatus, 
performHeads, 
performDiff, 
performLog, 
performCheckout, 
performCommit,
performCat,
performPull,
performPush) where

performInit :: String
performInit = "Initialized repo"

performClone :: String -> String
performClone repo_path = "Cloned"

performAdd :: [String] -> String
performAdd files = "Files added"

performRemove :: [String] -> String
performRemove files = "Files removed"

performStatus :: String
performStatus = "dvcs status output"

performHeads :: String
performHeads = "dvcs heads output"

performDiff :: String -> String -> String
performDiff revid1 revid2 = "dvcs diff output"

performLog :: String
performLog = "dvcs log output"

performCheckout :: String -> String
performCheckout revid = "Checked out"

performCommit :: String -> String
performCommit msg = "Committed"

performCat :: String -> String -> String
performCat revid file = "dvcs cat output"

performPull :: String -> String
performPull repo_path = "Pulled"

performPush :: String -> String
performPush repo_path = "Pushed"


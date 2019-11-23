import System.IO
import Test.HUnit
import Text.Printf

import BehaviorHiding.UserInteraction (parse)

import System.Process
import System.Environment (getArgs)

main = do
   args <- getArgs
   msg <- parse args  
   putStrLn $ msg

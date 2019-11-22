import System.IO
import Test.HUnit
import Text.Printf

import UserInteraction (parse)

import System.Process
import System.Environment (getArgs)

main = do
   args <- getArgs
   let msg = parse args  
   putStrLn $ msg

import System.IO
import BehaviorHiding.UserInteraction

import System.Environment (getArgs)

main = do
   args <- getArgs
   msg <- parse args  
   putStrLn msg

import System.IO
import BehaviorHiding.UserInteraction

import System.Environment (getArgs)

main = getArgs >>= parse >>= putStrLn

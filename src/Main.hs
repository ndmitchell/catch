

module Main where

import System
import Test
import Hite
import CmdLine


main = do args <- getArgs
          mainArgs args


mainArgs args = exec Hite.cmdLine args

{-


main = do x <- getArgs
          if null x
              then putStr helpMsg
              else case head x of
                       "-test" -> test (tail x)
                       "-case" -> runCase (tail x)
                       x -> putStrLn $ "Unknown command, " ++ x


helpMsg = unlines [
    "Catch - Case And Termination Checker for Haskell",
    "(C) Neil Mitchell 2004-2006",
    "http://www.cs.york.ac.uk/~ndm/projects/catch.php"
    ]



runCase [x] = do src <- readFile x
                 caseCheck (read src)
runCase _ = putStrLn "Expected, filename"
-}

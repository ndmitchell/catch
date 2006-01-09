

module Main where

import System
import CmdLine
import TextUtil


main :: IO ()
main = do args <- getArgs
          mainArgs args


mainCmd :: String -> IO ()
mainCmd x = mainArgs (splitList " " x)


mainArgs :: [String] -> IO ()
mainArgs args = exec args

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

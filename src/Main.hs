

module Main(main) where



main = do x <- getArgs
          if null x
              then putStrLn helpMsg
              else case head x of
                       "-test" -> test (tail x)
                       _ -> putStrLn "Unknown command"


helpMsg = unlines [
    "Catch - Case And Termination Checker for Haskell",
    "(C) Neil Mitchell 2004-2005",
    "http://www.cs.york.ac.uk/~ndm/projects/catch.php"
    ]


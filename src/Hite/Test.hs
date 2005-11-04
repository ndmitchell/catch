

module Hite.Test where


import Hite.Type
import Hite.Read
import Hite.Show
import Hite.Check
import Hite.Reachable
import Hite.Forward


readAndParse :: FilePath -> IO Hite
readAndParse file = do let f = if null file then "example_hite.txt" else file
                       x <- readFile f
                       return $ read x


testRead :: FilePath -> IO ()
testRead file = do h <- readAndParse file
                   print h

testCheck :: FilePath -> IO ()
testCheck file = do h <- readAndParse file
                    print $ check h

testReachable :: FilePath -> String -> IO ()
testReachable file main = do h <- readAndParse file
                             let m = if null main then "main" else main
                             print $ reachable main h

testForward :: FilePath -> IO ()
testForward = testFilter forward



testFilter :: (Hite -> Hite) -> FilePath -> IO ()
testFilter f file = do h <- readAndParse file
                       print $ f h

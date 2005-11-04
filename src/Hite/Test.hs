

module Hite.Test where


import Hite.Type
import Hite.Read
import Hite.Show
import Hite.Check
import Hite.Reachable


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

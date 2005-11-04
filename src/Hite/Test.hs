

module Hite.Test where


import Hite.Type
import Hite.Read
import Hite.Show


testRead :: FilePath -> IO ()
testRead file = do let f = if null file then "example_hite.txt" else file
                   x <- readFile f
                   let h = read x :: Hite
                       s = show h
                   putStrLn s

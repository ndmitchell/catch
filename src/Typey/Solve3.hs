
module Typey.Solve3(typeySolve3) where

import Typey.Type
import IO
import Hite



typeySolve3 :: String -> Handle -> Hite -> DataM SmallT -> Func2M -> IO Bool
typeySolve3 file hndl hite datam funcm =
    do
        outBoth "-- TYPES OF FUNCTIONS"
        outLn $ unlines [a ++ " :: " ++ show b | (a,b) <- funcm]
        error "typeySolve3, todo"
    where
        outLn = hPutStrLn hndl
        out = hPutStr hndl
        outBoth x = putStrLn x >> outLn x

        outDebug x = hPutStrLn stderr x >> out x

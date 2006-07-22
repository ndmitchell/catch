
module Typey.Solve3(typeySolve3) where

import Typey.Type
import Typey.Abstract
import Typey.Evaluate
import IO
import Hite
import General.General


typeySolve3 :: String -> Handle -> Hite -> DataM SmallT -> Func2M -> IO Bool
typeySolve3 file hndl hite datam funcm =
    do
        outBoth "-- TYPES OF FUNCTIONS"
        outLn $ unlines [a ++ " :: " ++ show b | (a,b) <- funcm]
        let mainT = getArgs $ lookupJust "main" funcm
            baseT = map (getAbstract datam) mainT
        outBoth $ "main args: " ++ intercatS " , " baseT
        outBoth "-- SOLVING"
        res <- evaluate outLn hite datam funcm baseT
        let List (Bit bot:_) = res
        outBoth $ "main result: " ++ show res
        outBoth $ if bot then "failure" else "success"
        return bot
    where
        outLn = hPutStrLn hndl
        out = hPutStr hndl
        outBoth x = putStrLn x >> outLn x

        outDebug x = hPutStrLn stderr x >> out x

        getArgs (Arr2T xs _) = xs
        getArgs x = []


module Typey.Solve2(typeySolve2) where

import Typey.Type
import Typey.Subtype
import Typey.CtorTypes
import Typey.FuncTypes
import Typey.Eliminate
import IO
import Hite
import General.General



typeySolve2 :: String -> Handle -> Hite -> DataM SmallT -> Func2M -> IO Bool
typeySolve2 file hndl hite datam funcm =
    do
        outBoth "-- TYPES OF FUNCTIONS"
        outLn $ unlines [a ++ " :: " ++ show b | (a,b) <- funcm]
        outBoth "-- SUBTYPES OF DATA"
        datat <- return $ ctorTypes datam
        outLn $ showTypeList datat
        outBoth "-- SUBTYPES OF FUNCTIONS"
        funct <- return $ funcTypes datam funcm
        outLn $ showTypeList funct
        outBoth "-- CALCULATING SUBTYPES"
        funct2 <- eliminate out hite datam datat funct
        outBoth "-- VALID SUBTYPES"
        outLn $ showTypeList funct2
        outBoth "-- ANSWER"
        outBoth $ showTypeList $ filter ((==) "main" . fst) funct2
        outBoth "-- SUMMARY"
        let failure = True -- any isTBot $ map snd $ lookupJust "main" funct2
        outBoth $ if failure then "Failed :(" else "Success :)"
        return $ not failure
    where
        outLn = hPutStrLn hndl
        out = hPutStr hndl
        outBoth x = putStrLn x >> outLn x

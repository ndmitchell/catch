
module Typey.Solve2(typeySolve2) where

import Typey.Type
import Typey.Subtype
import Typey.CtorTypes
import Typey.FuncTypes
import Typey.Eliminate
import IO
import Hite



typeySolve2 :: String -> Handle -> Hite -> DataM SmallT -> Func2M -> IO Bool
typeySolve2 file hndl hite datam funcm =
    do
        outBoth "-- TYPES OF FUNCTIONS"
        out $ unlines [a ++ " :: " ++ show b | (a,b) <- funcm]
        outBoth "-- SUBTYPES OF DATA"
        out $ showTypeList datat
        outBoth "-- SUBTYPES OF FUNCTIONS"
        out $ showTypeList funct
        outBoth "-- VALID SUBTYPES"
        out $ showTypeList funct2
        return False
    where
        datat = ctorTypes datam
        funct = funcTypes datam funcm
        funct2 = eliminate hite datam datat funct
    
        out = hPutStrLn hndl
        outBoth x = putStrLn x >> out x

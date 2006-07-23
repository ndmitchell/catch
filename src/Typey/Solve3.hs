
module Typey.Solve3(typeySolve3) where

import Typey.Type
import Typey.Abstract
import Typey.Evaluate
import IO
import Hite
import General.General
import Control.Monad


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
        let bot = absBottom res
        outBoth $ "main result: " ++ show res
        outBoth $ if bot then "Failure" else "Success"
        
        when bot $ do
            putStrLn "Searching for a minimal failing example"
            minimalFail hite datam funcm baseT
            return ()
        
        return bot
    where
        outLn = hPutStrLn hndl
        out = hPutStr hndl
        outBoth x = putStrLn x >> outLn x

        outDebug x = hPutStrLn stderr x >> out x

        getArgs (Arr2T xs _) = xs
        getArgs x = []


minimalFail :: Hite -> DataM SmallT -> Func2M -> [Abstract ()] -> IO [Abstract ()]
minimalFail hite datam funcm args = do
        putStrLn $ "From: " ++ show args
        f (minFails args)
    where
        f [] = do putStrLn $ "Res : " ++ show args
                  return args
        f (x:xs) = do
            putStrLn $ "Test: " ++ show x
            b <- testArgs x
            if b
                then minimalFail hite datam funcm x
                else f xs
        
        -- return True if you get bottom
        testArgs :: [Abstract ()] -> IO Bool
        testArgs args = do
            res <- evaluate (const $ return ()) hite datam funcm args
            return $ absBottom res


minFails :: [Abstract a] -> [[Abstract a]]
minFails x = tail $ crossProduct $ map minAbs x


minAbs :: Abstract a -> [Abstract a]
minAbs x = map (unflatten x) bs2 
    where
        bs2 = bs : reverse [bs !!! (i,False) | i <- [0..length bs-1], bs !! i]
        bs = flatten x


flatten :: Abstract a -> [Bool]
flatten (List _ xs ys) = concatMap flatten (xs++ys)
flatten (Bit x) = [x]
flatten _ = []


unflatten :: Abstract a -> [Bool] -> Abstract a
unflatten (List b xs ys) bs = List b (f xs xsb) (f ys ysb)
    where
        (xsb,ysb) = splitAt (length $ concatMap flatten xs) bs
        f [] [] = []
        f (x:xs) bs = unflatten x xb : f xs xsb
            where (xb,xsb) = splitAt (length $ flatten x) bs

unflatten (Bit x) [y] = Bit y
unflatten x [] = x


module Fixp(fixp, fixpVals) where

import Req
import Yhc.Core

-- PURE PART

fixp :: (Show v, Show k, Eq v, Eq k) => v -> (v -> v -> v) -> (k -> (k -> IO v) -> IO v) -> k -> IO v
fixp def merge solve key = gen [] key
    where
        -- look up in the stack if possible
        ask stack key = do
            -- putStrLn $ "Searching for: " ++ show key
            -- putStrLn $ "In: " ++ show [(show a, show b) | (a,b) <- stack]
            case lookup key stack of
                Nothing -> {- putStrLn "Not found" >> -} gen stack key
                Just x -> {- putStrLn "Found" >> -} return x
        
        -- create afresh
        gen stack key = f def
            where
                f value = do
                    value2 <- solve key (ask ((key,value):stack))
                    let value3 = merge value2 value
                    --putStrLn $ "1: " ++ show value
                    --putStrLn $ "2: " ++ show value2
                    --putStrLn $ "3: " ++ show value3
                    if value == value3 then {- putStrLn "*** FIXEDPOINT REACHED!" >> -} return value else f value3


-- REQ SPECIFIC PART

fixpVals :: Core -> Vals -> (ReqCall -> (ReqCall -> IO Vals) -> IO Vals) -> ReqCall -> IO Vals
fixpVals core def solve key = fixp def merge2 solve2 key
    where
        merge2 a b = normalise core $ blur core $ valsAnd core a b

        solve2 :: ReqCall -> (ReqCall -> IO Vals) -> IO Vals
        solve2 key onestep = do
            val <- solve key onestep
            return $ normalise core $ blur core val

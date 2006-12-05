
module Tram.Fixp(fixp, fixpReqs) where

import Tram.Req
import Data.Proposition
import Control.Monad


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
                    if value == value3 then return value else f value3


-- REQ SPECIFIC PART

-- work on Formula, but use BDD for the fixpoint value

fixpReqs :: Formula Req -> (Req -> (Req -> IO (Formula Req)) -> IO (Formula Req)) -> Req -> IO (Formula Req)
fixpReqs def solve key = liftM propRebuildFormula $ fixp (propRebuildBDD def) merge2 solve2 key
    where
        merge2 a b = propAnd (propSimplify $ propRebuildBDD $ propRebuildFormula $ propAnd a b) b
    
        solve2 :: Req -> (Req -> IO (BDD Req)) -> IO (BDD Req)
        solve2 key onestep = do
                oldValue <- onestep key
                newValue <- solve key onestep2
                return $ propSimplify $ propRebuildBDD $ blurReqs $ propAnd (propRebuildFormula oldValue) newValue
            where onestep2 x = liftM propRebuildFormula $ onestep x

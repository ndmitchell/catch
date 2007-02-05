
module Backward(backward) where

import Req
import Template
import Propagate
import Reduce
import System.IO
import Data.Proposition
import Data.Maybe
import Data.List
import Control.Monad
import Yhc.Core
import Safe
import qualified Data.Map as Map


-- basic idea:
-- store everything as a Map, the Vals for each item

type FixpMap = Map.Map CoreFuncName Vals


backward :: Core -> Template -> Handle -> Scopes -> IO Vals
backward core template hndl x = do
        newMap <- f origMap origTodo
        
        hPutStrLn hndl "Final:"
        mapM_ (hPutStrLn hndl) ["  " ++ k ++ " = " ++ show v | (k,v) <- Map.toList newMap]
        
        return $ Map.findWithDefault valsTrue "main" newMap
    where
        origMap = Map.fromList [(a, b) | Scope a b <- x]
        origTodo = Map.keys origMap
    
        f :: FixpMap -> [CoreFuncName] -> IO FixpMap
        f table [] = return table
        f table (x:xs) = do
            hPutStrLn hndl $ "Processing: " ++ x
            res <- oneStep $ Scope x $ fromJust $ Map.lookup x table
            
            let (todo2,table2) = foldr g ([], table) res
            if null todo2
                then hPutStrLn hndl $ "  Always safe"
                else mapM_ (outLine table2) todo2
            
            let (todome,todoother) = partition (== x) todo2
            
            f table2 (nub $ todome ++ todoother ++ xs)
            where
                outLine table x
                    = hPutStrLn hndl $ ("  " ++) $ show $ Scope x $ Map.findWithDefault valsTrue x table
        
        
        g :: Scope -> ([CoreFuncName], FixpMap) -> ([CoreFuncName], FixpMap)
        g (Scope func x) (todo,mp)
                | ans == ans2 = (todo,mp)
                | otherwise = (func:todo,Map.insert func ans2 mp)
            where
                ans = Map.findWithDefault valsTrue func mp
                ans2 = valsAnd core x ans

        oneStep :: Scope -> IO Scopes
        oneStep = mapM simple . propagate core
            
        simple :: (CoreFuncName, Reqs) -> IO Scope
        simple (name,x) = do
            x2 <- reducesWithM core (liftM propLit . templateGet template) x
            let x3 = collapse core x2
            return $ Scope name x3


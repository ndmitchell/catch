
module Backward(backward) where

import Req
import ReqEq
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
-- store everything as a Map, with a BDD of each Req
-- to process, first operation is to convert out of BDD
-- process, reencode as BDD and look for fixp

type FixpMap = Map.Map CoreFuncName Reqs


backward :: Core -> Template -> Handle -> Scopes -> IO Reqs
backward hill template hndl x = do
        newMap <- f origMap origTodo
        
        hPutStrLn hndl "Final:"
        mapM_ (hPutStrLn hndl) ["  " ++ k ++ " = " ++ show v | (k,v) <- Map.toList newMap]
        
        return $ Map.findWithDefault propTrue "main" newMap
    where
        origMap = Map.fromList [(a, b) | Scope a b <- x]
        origTodo = Map.keys origMap
    
        f :: FixpMap -> [CoreFuncName] -> IO FixpMap
        f table [] = return table
        f table (x:xs) = do
            hPutStrLn hndl $ "Processing: " ++ x
            res <- oneStep $ Scope x $ fromJustNote "Tram.Backward" $ Map.lookup x table
            
            let (todo2,table2) = foldr g ([], table) res
            if null todo2
                then hPutStrLn hndl $ "  Always safe"
                else mapM_ (outLine table2) todo2
            
            let (todome,todoother) = partition (== x) todo2
            
            f table2 (nub $ todome ++ todoother ++ xs)
            where
                outLine table x
                    = hPutStrLn hndl $ ("  " ++) $ show $ Scope x $ Map.findWithDefault propTrue x table

        g :: Scope -> ([CoreFuncName], FixpMap) -> ([CoreFuncName], FixpMap)
        g (Scope func x) (todo,mp)
                | ans `equalReqs` ans2 = (todo,mp)
                | otherwise = (func:todo,Map.insert func ans2 mp)
            where
                ans = Map.findWithDefault propTrue func mp
                ans2 = propSimplify $ propAnd x ans
        
        
        oneStep :: Scope -> IO Scopes
        oneStep = liftM blurScopes . mapM simple . propagate hill
            
        simple :: Scope -> IO Scope
        simple (Scope name x) = do
            x2 <- reducesWithM (templateGet template) $ propSimplify x
            return $ Scope name $ propSimplify x2

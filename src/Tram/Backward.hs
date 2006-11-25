
module Tram.Backward(backward) where

import Tram.Req
import Tram.Template
import Tram.Propagate
import Tram.Reduce
import System.IO
import Data.Proposition
import Data.Maybe
import Data.List
import Control.Monad
import Hill.All
import Safe
import qualified Data.Map as Map


-- basic idea:
-- store everything as a Map, with a BDD of each Req
-- to process, first operation is to convert out of BDD
-- process, reencode as BDD and look for fixp

type FixpMap = Map.Map FuncName (BDD Req)


backward :: Hill -> Template -> Handle -> Scopes Formula -> IO (Formula Req)
backward hill template hndl x = do
        newMap <- f origMap origTodo
        
        hPutStrLn hndl "Final:"
        mapM_ (hPutStrLn hndl) ["  " ++ k ++ " = " ++ show v | (k,v) <- Map.toList newMap]
        
        return $ propRebuildFormula $ Map.findWithDefault propTrue "main" newMap
    where
        origMap = Map.fromList [(a, propRebuildBDD b) | Scope a b <- x]
        origTodo = Map.keys origMap
    
        f :: FixpMap -> [FuncName] -> IO FixpMap
        f table [] = return table
        f table (x:xs) = do
            hPutStrLn hndl $ "Processing: " ++ x
            res <- oneStep $ Scope x $ propRebuildFormula $ fromJustNote "Tram.Backward" $ Map.lookup x table
            
            let (todo2,table2) = foldr g ([], table) res
            if null todo2
                then hPutStrLn hndl $ "  Always safe"
                else mapM_ (outLine table2) todo2
            
            f table2 (nub $ xs ++ todo2)
            where
                outLine table x
                    = hPutStrLn hndl $ ("  " ++) $ show $ Scope x $ Map.findWithDefault propTrue x table

        g :: Scope Formula -> ([FuncName], FixpMap) -> ([FuncName], FixpMap)
        g (Scope func x) (todo,mp)
                | ans == ans2 = (todo,mp)
                | otherwise = (func:todo,Map.insert func ans2 mp)
            where
                ans = Map.findWithDefault propTrue func mp
                ans2 = propSimplify $ propRebuildBDD $ propRebuildFormula $ propSimplify $ propAnd (propRebuildBDD x) ans
        
        
        oneStep :: Scope Formula -> IO (Scopes Formula)
        oneStep = liftM blurScopes . mapM simple . propagate hill
            
        simple :: Scope Formula -> IO (Scope Formula)
        simple (Scope name x) = do
            x2 <- reducesWithM (templateGet template) $ propSimplify x
            return $ Scope name $ propSimplify x2

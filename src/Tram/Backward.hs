
module Tram.Backward(backward) where

import Tram.Req
import Tram.Template
import Tram.Propagate
import Tram.Reduce
import Tram.Fixp
import System.IO
import Data.Proposition
import Data.Maybe
import Data.List
import Hill.All
import qualified Data.Map as Map


backward :: Hill -> Template -> Handle -> Scopes -> IO Reqs
backward hill template hndl x = do
		newMap <- f origMap origTodo
		return $ Map.findWithDefault propTrue "main" newMap
	where
        origMap = Map.fromList [(a,b) | Scope a b <- x]
        origTodo = Map.keys origMap
    
        f :: Map.Map FuncName Reqs -> [FuncName] -> IO (Map.Map FuncName Reqs)
        f table [] = return table
        f table (x:xs) = do
            hPutStrLn hndl $ "Processing: " ++ x
            res <- oneStep $ Scope x $ fromJust $ Map.lookup x table
            
            let (todo2,table2) = foldr g ([], table) res
            if null todo2
                then hPutStrLn hndl $ "  Always safe"
                else mapM_ (outLine table2) todo2
            
            f table2 (nub $ xs ++ todo2)
            where
                outLine table x
                    = hPutStrLn hndl $ ("  " ++) $ show $ Scope x $ Map.findWithDefault propTrue x table

        g (Scope func x) (todo,mp)
                | ans == ans2 = (todo,mp)
                | otherwise = (func:todo,Map.insert func ans2 mp)
            where
                ans = Map.findWithDefault propTrue func mp
                ans2 = propAnd x ans
        
        
        oneStep :: Scope -> IO Scopes
        oneStep scope = mapM simple $ propagate hill scope
            
        simple :: Scope -> IO Scope
        simple (Scope name x) = do
            x2 <- reducesWithM (templateGet template) $ propSimplify x
            return $ Scope name $ propSimplify x2

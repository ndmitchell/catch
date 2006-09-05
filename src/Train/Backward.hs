
module Train.Backward(backward) where

import Train.Type
import Train.Template
import Train.Propagate
import Train.Reduce
import Train.Fixp
import System.IO
import General.General
import Control.Monad
import Data.Proposition
import Data.Maybe
import Data.List
import Hite
import qualified Data.Map as Map


backward :: ZHite -> Template -> Handle -> Scopes -> IO Reqs
backward zhite template hndl x = do
		newMap <- f origMap origTodo
		return $ Map.findWithDefault propTrue "main" newMap
	where
        origMap = Map.fromList [(a,b) | Scope a b <- x]
        origTodo = Map.keys origMap
    
		--outBoth x = hPutStrLn hndl x >> putStrLn x
        
        f :: Map.Map FuncName Reqs -> [FuncName] -> IO (Map.Map FuncName Reqs)
        f table [] = return table
        f table (x:xs) = do
            res <- oneStep $ Scope x $ fromJust $ Map.lookup x table
            let (todo2,table2) = foldr g ([], table) res
            f table2 (nub $ xs ++ todo2)

        g (Scope func x) (todo,mp)
                | ans == ans2 = (todo,mp)
                | otherwise = (func:todo,Map.insert func ans2 mp)
            where
                ans = Map.findWithDefault propTrue func mp
                ans2 = propAnd x ans
        
        
        oneStep :: Scope -> IO Scopes
        oneStep scope = mapM simple $ propagate zhite scope
            
        simple :: Scope -> IO Scope
        simple (Scope name x) = do
            x2 <- reducesWithM (templateGet template) x
            return $ Scope name x2
            
{-        
        
	
		f scope = do
			scope2 <- backs scope
			fixp propTrue g scope2

		g (Scope "main" x) gen = return [Scope "main" x]

		g scope gen = do
			let scopes = propagate zhite scope
			res <- mapM (\x -> backs x >>= gen) scopes
			putStrLn $ "Propagate: " ++ output scope ++ " ==> " ++ output res
			return res
		
		backs (Scope name x) = do
			x2 <- reducesWithM (templateGet template) x
			return (Scope name x2)
	-}		
{-
-- solve all but propagate
backwardReqs :: ZHite -> Template -> Handle -> Reqs -> IO Reqs
backwardReqs zhite template hndl x = mapPredLitM f x
	where
		f x = mapPredLitM g (reduce x)
		
		g req@(Req hite expr path ctor) = case expr of
			ZVar y -> return $ predLit req
			ZCall name xs -> templateGet template req
-}

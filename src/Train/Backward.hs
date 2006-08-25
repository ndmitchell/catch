
module Train.Backward(backward) where

import Train.Type
import Train.Template
import Train.Propagate
import Train.Reduce
import Train.Fixp
import System.IO
import General.General
import Data.Predicate


backward :: ZHite -> Template -> Handle -> Scopes -> IO Scopes
backward zhite template hndl x = do
		outBoth $ "Solving: " ++ output x
		res <- mapPredLitM f x
		outBoth $ "Result: " ++ output res
		
		return res
	where
		outBoth x = hPutStrLn hndl x >> putStrLn x
	
		f scope = do
			scope2 <- backs scope
			fixp predTrue g scope2

		g (Scope "main" x) gen = return $ predLit $ Scope "main" x

		g scope gen = do
			let scopes = propagate zhite scope
			mapPredLitM (\x -> backs x >>= gen) scopes
		
		backs (Scope name x) = do
			x2 <- backwardReqs zhite template hndl x
			return (Scope name x2)
			
			
-- solve all but propagate
backwardReqs :: ZHite -> Template -> Handle -> Reqs -> IO Reqs
backwardReqs zhite template hndl x = mapPredLitM f x
	where
		f x = mapPredLitM g (reduce x)
		
		g req@(Req hite expr path ctor) = case expr of
			ZVar y -> return $ predLit req
			ZCall name xs -> templateGet template req

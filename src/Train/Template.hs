
module Train.Template(Template, templateInit, templateGet) where

import Train.Type
import Train.Reduce
import Train.Fixp
import System.IO
import Data.Predicate
import General.General
import Data.IORef
import Data.Char
import Control.Monad


data Template = Template ZHite Handle (IORef [(Req, Reqs)])


templateInit :: ZHite -> Handle -> IO Template
templateInit zhite hndl = do
	x <- newIORef []
	return $ Template zhite hndl x


-- first element of Req must be a ZCall
templateGet :: Template -> Req -> IO Reqs
templateGet (Template zhite hndl cache) req = do
	let abstract = templateAbstract req
	res <- readIORef cache
	ans <- case lookup abstract res of
			   Just x -> return x
			   Nothing -> do
			   	   ans <- templateCalc zhite hndl abstract
			   	   writeIORef cache ((abstract,ans):res)
			   	   hPutStrLn hndl $ "Add: " ++ output abstract ++ " = " ++ output ans
			   	   return ans
	return $ templateConcrete req ans



-- need to make it more abstract, and then more concrete, to satisfy the cache
templateAbstract :: Req -> Req
templateAbstract (Req a (ZCall name xs) b c) = Req a (ZCall name args) b c
	where args = map ZVar [['?',chr (i + ord 'a')] | i <- [0..length xs-1]]


templateConcrete :: Req -> Reqs -> Reqs
templateConcrete (Req _ (ZCall name args) _ _) y = mapPredLit (predLit . f) y
	where
		f (Req a b c d) = Req a (mapOver g b) c d
		g (ZVar ['?',x]) = args !! (ord x - ord 'a')
		g x = x


-- expr is ZCall
templateCalc :: ZHite -> Handle -> Req -> IO Reqs
templateCalc zhite hndl req = do
		putStrLn $ "templateCalc: " ++ output req
		res <- fixp predTrue f req
		return res
	where
		f req gen = do
			let reqs = instantiate zhite req
			mapPredLitM (g gen) reqs
		
		g gen req = do
			res <- mapPredLitM (h gen) (reduce req)
			res2 <- return $ predDnf res
			putStrLn $ "FROM: " ++ output req
			putStrLn $ "TO: " ++ output res2
			return res2
		
		h gen req@(Req hite expr path ctors) = case expr of
			ZCall name args -> do
				let abstract = templateAbstract req
				putStrLn $ "Calling gen: " ++ output abstract
				answer <- gen abstract
				putStrLn $ "Answer: " ++ output answer
				return $ templateConcrete req answer
			_ -> return $ predLit req


instantiate :: ZHite -> Req -> Reqs
instantiate (ZHite datas funcs) r1@(Req a (ZCall name args) b c) =
		mapPredLit rep $ predAnd $ concatMap f body
	where
		(args2, body) = head [(a,b) | ZFunc nam a b <- funcs, nam == name]
		
		f (cond, Left _) = []
		f (cond, Right body) = [predOr [predNot cond, predLit $ Req a body b c]]
		
		rep (Req a b c d) = predLit (Req a (mapOver g b) c d)
		
		g (ZVar x) = case lookup x (zip args2 args) of
						  Nothing -> ZVar x
						  Just y -> y
		g x = x
		
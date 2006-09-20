
module Train.Template(Template, templateInit, templateGet) where

import Train.Type
import Train.Reduce
import Train.Fixp
import System.IO
import Data.BDD
import General.General
import Data.IORef
import Data.Char
import Data.List
import Control.Monad
import Hite


data Template = Template ZHite Handle (IORef [(Req, Reqs)])


templateInit :: ZHite -> Handle -> IO Template
templateInit zhite hndl = do
	x <- newIORef []
	return $ Template zhite hndl x


-- first element of Req must be a ZCall
templateGet :: Template -> Req -> IO Reqs
templateGet template@(Template zhite hndl cache) req = do
    let abstract = templateAbstract req
    res <- readIORef cache
    ans <- case lookup abstract res of
               Just x -> return x
               Nothing -> do
                   ans <- templateCalc template zhite hndl abstract
                   modifyIORef cache ((abstract,ans):)
                   hPutStrLn hndl $ "Add: " ++ output abstract ++ " = " ++ output ans
                   return ans
    return $ templateConcrete req ans



-- need to make it more abstract, and then more concrete, to satisfy the cache
templateAbstract :: Req -> Req
templateAbstract (Req a (ZCall name xs) b c) = newReq a (ZCall name args) b c
	where args = map ZVar [['?',chr (i + ord 'a')] | i <- [0..length xs-1]]


templateConcrete :: Req -> Reqs -> Reqs
templateConcrete (Req _ (ZCall name args) _ _) y = mapBDD (bddLit . f) y
	where
		f (Req a b c d) = newReq a (mapOver g b) c d
		g (ZVar ['?',x]) = args !! (ord x - ord 'a')
		g x = x


-- expr is ZCall
templateCalc :: Template -> ZHite -> Handle -> Req -> IO Reqs
templateCalc template zhite hndl req = do
        hPutStrLn hndl $ "BEGIN: templateCalc, " ++ output req
        res <- liftM simplifyReqs $ fixp bddTrue f req
        hPutStrLn hndl $ "END  : templateCalc, " ++ output res
        return $ simplifyReqs res
    where
        parent = getFuncName req

        f req gen = do
            let reqs = instantiate zhite req
            reducesWithM (g gen) reqs

        -- can do it more efficiently, a fixp cut
        g gen req | parent `notElem` reachSet zhite (getFuncName req) =
            templateGet template req

        g gen req = do
            let abstract = templateAbstract req
            answer <- gen abstract
            res <- liftM simplifyReqs $ reducesWithM (g gen) (templateConcrete req answer)
            return res

{-			
			
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
-}


instantiate :: ZHite -> Req -> Reqs
instantiate (ZHite datas funcs) r1@(Req a (ZCall name args) b c) = res
	where
		res = bddAnds $ concatMap f body
	
		(args2, body) = head [(a,b) | ZFunc nam a b <- funcs, nam == name]
		
		f (cond, Left _) = []
		f (cond, Right body) = [propMapReduce rep $ reqsNot cond `bddOr` (newReqs a body b c)]
		
		rep (Req a b c d) = newReqs a (mapOver g b) c d
		
		g (ZVar x) = case lookup x (zip args2 args) of
						  Nothing -> error $ "Train.Template.instantiate: not found" -- ZVar x
						  Just y -> y
		g x = x



getFuncName :: Req -> FuncName
getFuncName (Req _ (ZCall nam _) _ _) = nam


reachSet :: ZHite -> FuncName -> [FuncName]
reachSet zhite name = fixSet f [name]
	where
		f :: FuncName -> [FuncName]
		f x = nub [y | (_, Right expr) <- body, ZCall y _ <- allOver expr]
			where ZFunc _ _ body = getZFunc zhite x

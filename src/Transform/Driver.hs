
module Transform.Driver(applyTransform) where

import Transform.Type
import Transform.Rewrite
import Transform.Normalise
import Data.Maybe
import General.General
import Control.Monad
import Data.List



applyTransform :: IHite -> IHite
applyTransform ihite = f ihite
	where
		f x = case applyFuncs x of
				  Nothing -> g x
				  Just x -> f x
		
		g x = case applyExprs x of
				  Nothing -> x
				  Just x -> f x


-- apply all the functions
-- Nothing implies a fixed point has been reached
-- Just means it hasn't, use this new value
applyFuncs :: IHite -> Maybe IHite
applyFuncs ihite@(IHite hite funcs) = liftM normalHite $ f $ allItems funcs
	where
		f [] = Nothing
		f ((pre,x,post):rest) = case funcTweak ihite x of
			Nothing -> f rest
			Just (x2,modify) -> Just $ IHite hite $ applyAll modify (pre++[x2]++post)

		applyAll g funcs = [normaliseIFunc func{funcExpr=mapIExpr g (funcExpr func)} | func <- funcs]


getName :: [IFunc] -> FuncName -> FuncName
getName funcs name = joinName name n
	where n = 1 + maximum (-1 : [b | func <- funcs, let (a,b) = splitName (funcName func), a == name])


splitName :: FuncName -> (String, Int)
splitName xs = (a, if null b then 0 else read (tail b))
	where (a,b) = break (== '~') xs

joinName :: String -> Int -> FuncName
joinName xs 0 = xs
joinName xs i = xs ++ "~" ++ show i



applyExprs :: IHite -> Maybe IHite
applyExprs ihite@(IHite hite funcs) = f False [] funcs
	where
		f False acc [] = Nothing
		f True acc [] = Just $ IHite hite (reverse acc)
		
		f change acc (x:xs) = case applyExpr ihite (funcExpr x) of
			None -> f change (x:acc) xs
			Change y -> f True (normaliseIFunc x{funcExpr=y} :acc) xs
			Insert func rep -> Just $ IHite hite
					(normaliseIFunc func{funcName=newname}:x{funcExpr=rep newname}:acc++xs)
				where newname = getName funcs (funcName func)
		



applyExpr :: IHite -> IExpr -> Alteration
applyExpr ihite expr = f False [] children
	where
		children = getChildren expr
		
		f change acc [] = case exprTweak ihite x2 of
				None -> if change then Change x2 else None
				x -> x
			where
				x2 = setChildren expr (reverse acc)

		f change acc (x:xs) = case applyExpr ihite x of
			None -> f change (x:acc) xs
			Change x -> f True (x:acc) xs
			Insert func rep -> Insert func
				(\name -> setChildren expr (take (length acc) children ++ [rep name] ++ xs))



-- if two functions are equal, reduce them to one
normalHite :: IHite -> IHite
normalHite (IHite datas funcs) =
		reachHite $ IHite datas [func{funcExpr=mapIExpr h (funcExpr func)} | func <- funcs2]
	where
		reps2 = concat reps
		(funcs2, reps) = unzip $ concatMap f $ groupSetExtract (fst . splitName . funcName) funcs
		
		f xs = map g $ groupSetExtract (\(Func _ a b) -> (a,b)) xs
		
		g [x] = (x,[])
		g (x:xs) = (x,[(funcName y,funcName x) | y <- xs])

		h (Call name xs) = case lookup name reps2 of
								Nothing -> Call name xs
								Just x -> Call x xs
		h x = x


reachHite :: IHite -> IHite
reachHite ihite@(IHite datas funcs) = IHite datas $ filter (\x -> funcName x `elem` reach) funcs
	where
		mainSet = [name | func <- funcs, let name = funcName func, fst (splitName name) == "main"]
		
		reach = fixSet f mainSet
		
		f x = nub [nam | Call nam _ <- allIExpr $ funcExpr $ getFunc ihite x]

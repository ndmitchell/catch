
module Transform.Driver(applyTransform) where

import Transform.Type
import Transform.Rewrite
import Transform.Normalise
import Data.Maybe
import General.General
import Control.Monad
import Data.List

---------------------------------------------------------------------
-- DRIVER

applyTransform :: IHite -> IHite
applyTransform ihite = maybe ihite id (apply ihite)
	where
		apply = fixMay item
		
		item = (applyFuncCreate <*> fixMay (fixMay applyExprTweak <*> applyFuncTweak))


fixMay :: (a -> Maybe a) -> (a -> Maybe a)
fixMay f x = case f x of
				 Nothing -> Nothing
				 Just a -> case fixMay f a of
				 			   Nothing -> Just a
				 			   Just q -> Just q


(<*>) :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
(<*>) g f x = case f x of
				Nothing -> g x
				Just x -> case g x of
					Nothing -> Just x
					Just x -> Just x


---------------------------------------------------------------------
-- TRANSFORMS

applyFuncCreate :: IHite -> Maybe IHite
applyFuncCreate ihite@(IHite hite funcs) =
		liftM normalHite $ case f False funcs [] funcs of
			(True, a) -> Just $ IHite hite a
			(False,a) -> Nothing
	where
		f :: Bool -> [IFunc] -> [IFunc] -> [IFunc] -> (Bool, [IFunc])
		f changed oldfuncs newfuncs [] = (changed,reverse newfuncs)
		f changed oldfuncs newfuncs (t:odo) =
			case applyCreate ihite t of
				Nothing -> f changed oldfuncs (t:newfuncs) odo
				Just (newfunc,oldfunc) ->
					case askFunc newfunc oldfuncs of
						Just name -> f True oldfuncs (oldfunc name : newfuncs) odo
						Nothing -> f True (newfunc2:oldfuncs) (oldfunc newname : newfuncs) (newfunc2:odo)
							where
								newname = getName oldfuncs (funcName newfunc)
								newfunc2 = newfunc{funcName = newname}


-- does the function already exist, under a different name
askFunc :: IFunc -> [IFunc] -> Maybe FuncName
askFunc (Func name args body) funcs =
		listToMaybe [name2 | Func name2 args2 body2 <- funcs,
					         fst (splitName name2) == str, args == args2, body == body2]
	where (str,num) = splitName name



applyCreate :: IHite -> IFunc -> Maybe (IFunc, FuncName -> IFunc)
applyCreate ihite (Func name args body) =
	case f body of
		Nothing -> Nothing
		Just (a,b) -> Just (normaliseIFunc a, \nam -> normaliseIFunc (Func name args (b nam)))
	where
		f expr = case g [] children of
					 Nothing -> funcCreate ihite expr
					 Just (a,b) -> Just (a, \nam -> setChildren expr (b nam))
			where
				children = getChildren expr
	
		g acc [] = Nothing
		g acc (x:xs) = case f x of
						   Nothing -> g (x:acc) xs
						   Just (a,b) -> Just (a, \nam -> reverse acc ++ b nam : xs)



-- apply all the functions
-- Nothing implies a fixed point has been reached
-- Just means it hasn't, use this new value
applyFuncTweak :: IHite -> Maybe IHite
applyFuncTweak ihite@(IHite hite funcs) = liftM normalHite $ f $ allItems funcs
	where
		f [] = Nothing
		f ((pre,x,post):rest) = case funcTweak ihite x of
			Nothing -> f rest
			Just (x2,modify) -> Just $ IHite hite $ applyAll modify (pre++[x2]++post)

		applyAll g funcs = [normaliseIFunc func{funcExpr=mapIExpr g (funcExpr func)} | func <- funcs]




applyExprTweak :: IHite -> Maybe IHite
applyExprTweak ihite@(IHite hite funcs) = liftM normalHite $ f False [] funcs
	where
		f False acc [] = Nothing
		f True acc [] = Just $ IHite hite (reverse acc)
		
		f change acc (x:xs) = case applyExpr ihite (funcExpr x) of
			Nothing -> f change (x:acc) xs
			Just y -> f True (normaliseIFunc x{funcExpr=y} :acc) xs
		



applyExpr :: IHite -> IExpr -> Maybe IExpr
applyExpr ihite expr = f False [] children
	where
		children = getChildren expr
		
		f change acc [] = case exprTweak ihite x2 of
				Nothing -> if change then Just x2 else Nothing
				x -> x
			where
				x2 = setChildren expr (reverse acc)

		f change acc (x:xs) = case applyExpr ihite x of
			Nothing -> f change (x:acc) xs
			Just x -> f True (x:acc) xs



---------------------------------------------------------------------
-- SMALL UTILITY

getName :: [IFunc] -> FuncName -> FuncName
getName funcs name = joinName nam n
	where
		n = 1 + maximum (-1 : [b | func <- funcs, let (a,b) = splitName (funcName func), a == nam])
		nam = fst $ splitName name


splitName :: FuncName -> (String, Int)
splitName xs = (a, if null b then 0 else read (tail b))
	where (a,b) = break (== '~') xs

joinName :: String -> Int -> FuncName
joinName xs 0 = xs
joinName xs i = xs ++ "~" ++ show i


---------------------------------------------------------------------
-- NORMALISATION AND REACHABILITY

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

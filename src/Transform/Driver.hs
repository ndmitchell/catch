
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
applyTransform ihite = res
	where
		res = maybe ihite id (apply ihite)
		apply = fixMay item
		item = applyFuncTweak <*> fixMay applyExprTweak


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

{-

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
askFunc :: FuncName -> Tweak -> [IFunc] -> Maybe FuncName
askFunc name tweak  funcs =
	where
		getFunc (IHite undefined funcs) 

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
applyFuncTweak ihite@(IHite hite funcs) = liftM normalHite $ f [] funcs
	where
		f acc [] = reverse
		
		f acc (x:xs) = case funcTweak ihite of
			
	
		f [] = Nothing
		f ((pre,x,post):rest) = case funcTweak ihite x of
			Nothing -> f rest
			Just (x2,modify) -> Just $ IHite hite $ applyAll modify (pre++maybeToList x2++post)

		applyAll g funcs = [normaliseIFunc func{funcExpr=mapIExpr g (funcExpr func)} | func <- funcs]



newFuncName :: [IFunc]

-}

applyFuncTweak :: IHite -> Maybe IHite
applyFuncTweak ihite@(IHite hite funcs) = liftM normalHite $ f False [] funcs
	where
		f changed acc [] = if changed then Just (IHite hite (reverse acc)) else Nothing
		
		f changed acc (x:xs) = case applyFuncAny ihite2 x of
			Nothing -> f changed (x:acc) xs
			Just (x2,name,tweak,newfunc) ->
					case lookup tweak $ funcTweaks $ getFunc ihite2 name of
						Just newname -> f True (rep newname x2 : acc) xs
						Nothing -> f True (addTweak name tweak newname (rep newname x2 : acc))
										  (addTweak name tweak newname (rep newname newfunc{funcName=newname} : xs))
							where
								newname = getName (acc++x:xs) name
						
			where
				ihite2 = IHite hite (acc++x:xs)

		rep newname func = normaliseIFunc func{funcExpr = mapIExpr g (funcExpr func)}
			where
				g (Call "" xs) = Call newname xs
				g x = x

		addTweak name tweak newname xs = map g xs
			where
				g x | funcName x == name = x{funcTweaks = (tweak,newname) : funcTweaks x}
					| otherwise = x


-- take a function, and return Just (newversion, tweaked, tweak, newversion)
applyFuncAny :: IHite -> IFunc -> Maybe (IFunc, FuncName, Tweak, IFunc)
applyFuncAny ihite func@(Func name args body tweaks) | isJust res
		= Just (Func name args newbody tweaks,name,tweak,newfunc)
	where
		Just (newbody,tweak,newfunc) = res
		res = funcTweak ihite func

applyFuncAny ihite func@(Func name args body tweaks) = do
		(newexpr,a,b,c) <- f body
		return (Func name args newexpr tweaks,a,b,c)
	where
		f expr = case g $ allItems $ getChildren expr of
					 Nothing -> funcCreate ihite expr
					 Just (newchildren,a,b,c) -> Just (setChildren expr newchildren,a,b,c)

		g [] = Nothing
		g ((pre,x,post):rest) = case f x of
			Nothing -> g rest
			Just (newexpr,a,b,c) -> Just (pre++newexpr:post,a,b,c)
					 




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

{-
-- if two functions are equal, reduce them to one
-- MAY NOT BE TWEAK SAFE, HENCE DO NOT USE
-}
uniqueBad ihite@(IHite datas funcs) =
		applyMerge ihite reps
	where
		(funcs2, reps) = unzip $ concatMap f $ groupSetExtract (fst . splitName . funcName) funcs
		
		f xs = map g $ groupSetExtract (\(Func _ a b _) -> (a,b)) xs
		
		g [x] = (x,[funcName x])
		g (x:xs) = (x, funcName x : map funcName xs)


normalHite :: IHite -> IHite
normalHite = reachHite . uniqueBad


reachHite :: IHite -> IHite
reachHite ihite@(IHite datas funcs) = res
	where
		reach = fixSet f ["main"]
		res = IHite datas [g func | func <- funcs, funcName func `elem` reach]
		f x = nub [nam | Call nam _ <- allIExpr $ funcExpr $ getFunc ihite x]
		
		g func = func{funcTweaks = [(a,b) | (a,b) <- funcTweaks func, b `elem` reach]}


-- perserves the tweak information
uniqueHite :: IHite -> IHite
uniqueHite ihite@(IHite datas funcs) =
		reachHite $ applyMerge ihite (map box rawSets ++ minSets)
	where
		-- ((a,b):xs) a == b, if all xs hold
		depPairs :: [[(FuncName,FuncName)]]
		depPairs = concatMap f $ groupSetExtract (fst . splitName . funcName) funcs
		
		f xs = map (map sortPair) $ mapMaybe (uncurry equalGiven) $ allPairs xs
		
		sortPair (a,b) | a > b = (b,a)
					   | otherwise = (a,b)
		
		minSets = foldr addSet [] $ map (\(a,b) -> [a,b]) $ map head $ fix remPairs depPairs
		rawSets = filter (`notElem` concat minSets) (map funcName funcs)
		
		remPairs pairs = filter g pairs
			where
				valid = map head pairs
				g (prove:require) = all (`elem` valid) require
		
		
		addSet :: [FuncName] -> [[FuncName]] -> [[FuncName]]
		addSet xs (y:ys) | null (xs `intersect` y) = y : addSet xs ys
						 | otherwise = nub (xs++y) : ys
		addSet xs [] = [xs]
		
		
	{-	
	
		reps2 = concat reps
		(funcs2, reps) = unzip $ concatMap f $ groupSetExtract (fst . splitName . funcName) funcs
		
		f xs = map g $ groupSetExtract (\(Func _ a b _) -> (a,b)) xs
		
		g [x] = (x,[])
		g (x:xs) = (x,[(funcName y,funcName x) | y <- xs])

		h (Call name xs) = case lookup name reps2 of
								Nothing -> Call name xs
								Just x -> Call x xs
		h x = x
-}

equalGiven :: IFunc -> IFunc -> Maybe [(FuncName, FuncName)]
equalGiven (Func name1 args1 body1 _) (Func name2 args2 body2 _)
	| args1 /= args2 = Nothing
	| otherwise = do res <- f body1 body2 ; return ((name1,name2):res)
	where
		f :: IExpr -> IExpr -> Maybe [(FuncName, FuncName)]
		f (Var a) (Var b) | a == b = Just []
		f (Make a xs) (Make b ys) | a == b = fs xs ys
		f (Call a xs) (Call b ys) | a == b = fs xs ys
								  | fst (splitName a) == fst (splitName b) =
			case fs xs ys of {Nothing -> Nothing; Just res -> Just ((a,b):res)}
		f (Case a xs) (Case b ys) | map fst xs == map fst ys = fs (a:map snd xs) (b:map snd ys)
		f (Lambda a x) (Lambda b y) | a == b = f x y
		f (Apply a xs) (Apply b ys) = fs (a:xs) (b:ys)
		f (Sel x a) (Sel y b) | a == b = f x y
		f (Error _) (Error _) = Just []
		f (Unknown) (Unknown) = Just []
		f _ _ = Nothing

		fs xs ys = gs [] xs ys
		
		gs acc [] [] = Just acc
		gs acc (x:xs) (y:ys) = case f x y of
									Nothing -> Nothing
									Just res -> gs (res++acc) xs ys
		gs acc _ _ = Nothing -- Call to name~1 vs name~2

-- only the first one in each set is the main one
-- all others get dropped
applyMerge :: IHite -> [[FuncName]] -> IHite
applyMerge ihite@(IHite datas funcs) merges = IHite datas (map f merges)
	where
		f set@(name:_) = Func name args (mapIExpr g body) $
						 nub [(a,repStr b) | (a,b) <- concatMap (funcTweaks . getFunc ihite) set]
			where Func _ args body _ = getFunc ihite name
		
		g (Call x xs) = Call (repStr x) xs
		g x = x

		repStr :: FuncName -> FuncName
		repStr x = head $ head $ filter (x `elem`) merges

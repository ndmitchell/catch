
module Transform.Driver(applyTransform) where

import Transform.Type
import Transform.Rewrite
import Transform.Normalise
import Data.Maybe
import General.General
import Control.Monad
import Data.List
import Debug.Trace


-- should details be Debug.Trace'd
logFuncCreate = False

mayTrace msg x = if logFuncCreate then trace msg x else x

---------------------------------------------------------------------
-- DRIVER

applyTransform :: IHite -> IHite
applyTransform ihite = reachHite res
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

applyFuncTweak :: IHite -> Maybe IHite
applyFuncTweak ihite@(IHite hite funcs) = liftM normalHite $ f False [] funcs
	where
		f changed acc [] = if changed then Just (IHite hite (reverse acc)) else Nothing
		
		f changed acc (x:xs) = case applyFuncAny ihite2 x of
			Nothing -> f changed (x:acc) xs
			Just (x2,name,tweak,newfunc) ->
					case lookup tweak $ funcTweaks $ getFunc ihite2 name of
						Just newname -> mayTrace ("Using " ++ newname ++ " as " ++ name ++ " " ++ show tweak) $
										f True (rep newname x2 : acc) xs
						Nothing -> mayTrace ("Creating " ++ newname ++ " as " ++ name ++ " " ++ show tweak) $
								   f True (addTweak name tweak newname (rep newname x2 : acc))
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
-- bad version of uniqueness
uniqueBad ihite@(IHite datas funcs) =
		applyMerge ihite reps
	where
		(funcs2, reps) = unzip $ concatMap f $ groupSetExtract (fst . splitName . funcName) funcs
		
		f xs = map g $ groupSetExtract (\(Func _ a b _) -> (a,b)) xs
		
		g [x] = (x,[funcName x])
		g (x:xs) = (x, funcName x : map funcName xs)
-}


normalHite :: IHite -> IHite
normalHite = uniqueHite

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
		applyMerge ihite (map box rawSets ++ minSets)
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
		addSet xs (y:ys) | disjoint xs y = y : addSet xs ys
						 | otherwise = nub (xs++y) : ys
		addSet xs [] = [xs]


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

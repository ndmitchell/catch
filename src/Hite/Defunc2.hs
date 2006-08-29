
module Hite.Defunc2(cmd) where

import Hite.Type
import General.General
import Hite.Normalise
import Data.List


cmd = cmdHitePure (const defunc2) "defunc2"
            "Perform Reynold's style defunctionalisation (2)"


defunc2 :: Hite -> Hite
defunc2 bad_hite = decode $ f $ encode hite
	where
		f = fix (reachy . specialise . fix (promote . arityRaise))
	
		hite = normalise bad_hite


encode :: Hite -> Hite
encode hite = mapExpr f hite
	where
	
		-- do not double defunctionalise
		f expr@(Call (CallFunc ('%':_)) _) = expr
	
		f expr@(Call (CallFunc x) xs) = case compare nargs nxs of
				EQ -> expr
				GT -> mkCtor x xs
				LT -> mkCall (mkCtor x (take nargs xs)) (drop nargs xs)
			where
				nxs = length xs
				nargs = getArity hite x
		
		f (Call x xs) = Call (CallFunc $ toCall (length xs)) (x:xs)
		
		f x = x


decode :: Hite -> Hite
decode hite = if null calls2 && null makes2 then hite else error $ show (calls2, makes2) ++ "\n" ++ output hite
	where
		calls2 = g calls
		makes2 = g makes
		(calls,makes) = unzip $ map f $ allExpr hite
		
		g x = (nub . sort . concat) x
		
		f (CallFunc x) | head x == '%' = ([fromCall x], [])
		f (Make x xs) | head x == '%' = ([], [fromCtor x])
		f x = ([], [])



-- do reachability, with the constructor encoding
-- do not reachability on data/constructors
reachy :: Hite -> Hite
reachy hite = hite{funcs = filter (\x -> funcName x `elem` keep) (funcs hite)}
	where
		keep = fixSet f ["main"]
		
		f x = nub $ concatMap g $ allExpr $ body $ getFunc hite x
		
		g (CallFunc x) | head x /= '%' = [x]
		g (Make x _) | head x == '%' = [fst $ fromCtor x]
		g _ = []



-- f args = Make %n args  ==>  f args newargs = Call n args newargs
arityRaise :: Hite -> Hite
arityRaise hite = mapFunc f hite
	where
		
		f func@Func{funcArgs=fargs, body=Make name@('%':_) args} 
				| add > 0 = func{funcArgs=fargs ++ newArgs, body=Call (CallFunc str) (args ++ map Var newArgs)}
			where
				newArgs = take add [arg | i <- [1..], let arg = "arg_" ++ show i, not (arg `elem` fargs)]
				add = arity - num
				(str,num) = fromCtor name
				arity = getArity hite str

		f x = x


-- (%ap Make xs) ys ==> %ap Make xs ys
promote :: Hite -> Hite
promote hite = mapExpr f hite
	where
		f (Call (CallFunc ('%':app)) (Make name xs : ys))
				| not (null ysy) = mkCall mk ysn
			where
				mk = if length xs2 == astr
					 then Call (CallFunc str) xs2
					 else mkCtor str xs
				xs2 = xs++ysy
			
				(ysy,ysn) = splitAt (astr-num) ys
				(str,num) = fromCtor name
				astr = getArity hite str

		f x = x


-- a function to specialise
data Spec = Spec {specName :: FuncName, specArgs :: [SpecArg]} deriving (Eq, Show)
data SpecArg = SpecArg {specPos :: Int, specFunc :: FuncName, specFrees :: Int} deriving (Eq, Show)


specialise :: Hite -> Hite
specialise hite = useSpec
	where
		askSpec :: [Spec]
		askSpec = nub $ concatMap f (allExpr hite)
			where
				f (Call (CallFunc name) xs)
					| head name /= '%' && not (null spec) = [Spec name spec]
					where
						spec = [SpecArg pos str num | (pos, Make nam _) <- zip [0..] xs,
									head nam == '%', let (str,num) = fromCtor nam]
		
				f x = []
		
		-- requires askSpec
		nameSpec :: [(FuncName, Spec)]
		nameSpec = concatMap f $ groupSetExtract specName askSpec
			where
				used = map funcName $ funcs hite
				f xs = zip [newname | i <- [1..], let newname = name ++ "_s" ++ show i, not (newname `elem` used)] xs
					where name = specName $ head xs
			
		
		
		-- requires nameSpec
		genSpec :: [Func]
		genSpec = map f nameSpec
			where
				f (newname, Spec oldname specargs) = 
						Func newname (concat newArgs) newbody q
					where
						newbody = mapExpr (g (zip oldargs reps)) oldbody
						(newArgs,reps) = unzip $ zipWith f [0..length oldargs - 1] freeArgPrefix
					
						freeArgPrefix = [x | i <- [1..], let x = "arg_" ++ show i ++ "_",
														 not $ any (x `isPrefixOf`) oldargs]
						Func _ oldargs oldbody q = getFunc hite oldname
						
						f n prefix = case filter (\x -> specPos x == n) specargs of
							[] -> let x = oldargs !! n in ([x], Var x)
							[x] -> let frees = map (\a -> prefix ++ show a) [1..specFrees x]
								   in (frees, mkCtor (specFunc x) (map Var frees))

						g rep (Var x) = case lookup x rep of
											Nothing -> Var x
											Just y -> y
						g rep x = x

		
		-- requires nameSpec and genSpec
		useSpec :: Hite
		useSpec = mapExpr f (hite{funcs = genSpec ++ funcs hite})
			where
				rep = map (\(a,b) -> (b,a)) nameSpec
			
				f (Call (CallFunc name) xs)
					| head name /= '%' && not (null spec) = Call (CallFunc newname) (concatMap g xs)
					where
						newname = lookupJust (Spec name spec) rep
						spec = [SpecArg pos str num | (pos, Make nam _) <- zip [0..] xs,
									head nam == '%', let (str,num) = fromCtor nam]
		
						g (Make nam xs) | head nam == '%' = xs
						g x = [x]
		
				f x = x









getArity :: Hite -> String -> Int
getArity hite = length . funcArgs . getFunc hite


mkCtor :: String -> [Expr] -> Expr
mkCtor x xs = Make (toCtor x (length xs)) xs

toCtor :: String -> Int -> String
toCtor x n = "%" ++ x ++ "_" ++ show n

fromCtor :: String -> (String, Int)
fromCtor ('%':xs) = (reverse str, read $ reverse num)
	where (num,'_':str) = break (== '_') $ reverse xs


mkCall :: Expr -> [Expr] -> Expr
mkCall x [] = x
mkCall x xs = Call (CallFunc $ toCall (length xs)) (x:xs)

toCall :: Int -> String
toCall n = '%' : show n

fromCall :: String -> Int
fromCall ('%':n) = read n

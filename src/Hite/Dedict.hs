
module Hite.Dedict(cmd) where

import Hite.Type
import Hite.Eq
import Hite.Normalise
import Hite.Reachable(reachable)
import General.General
import Data.Maybe
import Data.List
import qualified Data.Map as Map


cmd = cmdHitePure (const dedict) "dedict"
            "Make the dictionary transformation not use tuples"



dedict :: Hite -> Hite
dedict hite = fix detup $ normalise hite



type Tup = (FuncName, (TupPos, TupSize))
type TupPos = Int
type TupSize = Int


detup :: Hite -> Hite
detup hite = reachable "main" $ mapExpr useDetup (hite{funcs=genDetup++funcs hite})
	where
		tupleFunc :: Map.Map FuncName [(TupPos,TupSize)]
		tupleFunc = Map.fromList $ map f $ groupSetExtract fst $ fix (firstTup hite) []
			where f xs = (fst $ head xs, map snd xs)
		
		
		getName :: FuncName -> [(TupPos,TupSize)] -> FuncName
		getName name xs = name ++ "_detup#" ++ concatMap (('_':) . show . fst) xs
		
		genDetup :: [Func]
		genDetup = concatMap f $ Map.toList tupleFunc
			where
				funcNames = map funcName (funcs hite)
			
				f (name,xs) = concatMap (g name) (powerSet xs)
				
				g name [] = []
				g name specs
					| newname `elem` funcNames = []
					| otherwise = [Func newname (concat nargs) newbody q]
					where
						newname = getName name specs
						Func _ args body q = getFunc hite name
						
						(rep,nargs) = unzip $ [maybe (Var ai,[ai])
									  (\x -> let y = getargs ai x in (Make ("Tup" ++ show x) (map Var y), y))
								      (lookup i specs)
						              | i <- [0..length args-1], let ai = args !! i]
						
						getargs prefix n = [prefix ++ "_tup_" ++ show i | i <- [1..n]]
						
						newbody = mapExpr doRep body
						
						rep2 = zip args rep
						doRep (Var x) = case lookup x rep2 of
											Nothing -> Var x
											Just y -> y
						doRep x = x


		useDetup (Call (CallFunc name) xs)
			| not (null specs3)
			= Call (CallFunc $ getName name specs3) (concat args2)
			where
				specs3 = catMaybes specs2
				(specs2, args2) = unzip $ zipWith f [0..] xs
				specs = Map.findWithDefault [] name tupleFunc
				
				f n arg | isJust spc && tupSize == length tup =
						(Just (n, tupSize), tup)
					where
						tup = getTuple arg
						Just tupSize = spc
						spc = lookup n specs

				f n arg = (Nothing, [arg])

		useDetup x = x

		
		-- return Null if no tuple info
		getTuple :: Expr -> [Expr]
		getTuple (CallFunc name) | null $ funcArgs func = getTuple $ body func
			where func = getFunc hite name
		
		getTuple (Call x []) = getTuple x
		getTuple (Make ('T':'u':'p':_) xs) = xs
		getTuple x = []



-- given that the follow funcs have a tuple argument
-- which other functions must do
-- function, arg pos, tuple size
firstTup :: Hite -> [Tup] -> [Tup]
firstTup hite seen = nub $ sort $ seen ++ newseen
	where
		newseen = concatMap f $ funcs hite
	
		f (Func name args body _) = nub [(name, (fromJust $ lookup a $ zip args [0..], b))
										| (a,b) <- concatMap g $ allExpr body, b /= 0]

		g (Case (Var a) [('T':'u':'p':n, _)]) = [(a, read n :: Int)]
		
		g (Call (CallFunc name) as) = concat $ zipWith h [0..] as
			where
				h i (Var a) = case [t3 | (t1,(t2,t3)) <- seen, t1 == name, t2 == i] of
								   [i] -> [(a,i)]
								   [] -> []
				h _ _ = []

		g _ = []

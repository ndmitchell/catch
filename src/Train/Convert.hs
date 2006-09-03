
module Train.Convert where

import Hite
import Train.Type
import General.General
import Data.Predicate
import Data.Proposition


convertHite :: Hite -> ZHite
convertHite hite@(Hite datas funcs) = ZHite datas (concatMap (convertFunc hite) funcs)


convertFunc :: Hite -> Func -> [ZFunc]
convertFunc hite (Func "_" _ _ _) = []
convertFunc hite (Func name args body _) = [ZFunc name args (convertBody hite body)]


convertBody :: Hite -> Expr -> [(Reqs, Either String ZExpr)]
convertBody hite (MCase alts) = [(foldPred propOrs propAnds fone a, convertExpr b) | MCaseAlt a b <- alts]
	where
		fone (MCaseOpt x c) = newReqs hite (fromRight $ convertExpr x) (emptyPath hite) [c]
	
convertBody hite x = [(propTrue, convertExpr x)]


convertExpr :: Expr -> Either String ZExpr
convertExpr (Error x) = Left x
convertExpr x = Right $ case x of
		Call (CallFunc "_") [] -> ZAny
		Call (CallFunc x) xs -> ZCall x (fs xs)
		Make x xs -> ZMake x (fs xs)
		Var x -> ZVar x
		Sel x y -> ZSel (f x) y
		Unknown -> ZAny
	where
		f = fromRight . convertExpr
		fs = map f

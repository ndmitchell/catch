
module Train.Convert where

import Hite
import Train.Type
import General.General
import Data.Predicate


convertHite :: Hite -> ZHite
convertHite hite@(Hite datas funcs) = ZHite datas (map (convertFunc hite) funcs)


convertFunc :: Hite -> Func -> ZFunc
convertFunc hite (Func name args body _) = ZFunc name args (convertBody hite body)


convertBody :: Hite -> Expr -> [(Reqs, Either String ZExpr)]
convertBody hite (MCase alts) = [(mapPredLit f a, convertExpr b) | MCaseAlt a b <- alts]
	where f (MCaseOpt x c) = predLit $ Req hite (fromRight $ convertExpr x) emptyPath [c]
convertBody hite x = [(predTrue, convertExpr x)]


convertExpr :: Expr -> Either String ZExpr
convertExpr (Error x) = Left x
convertExpr x = Right $ case x of
		Call (CallFunc x) xs -> ZCall x (fs xs)
		Make x xs -> ZMake x (fs xs)
		Var x -> ZVar x
		Sel x y -> ZSel (f x) y
	where
		f = fromRight . convertExpr
		fs = map f

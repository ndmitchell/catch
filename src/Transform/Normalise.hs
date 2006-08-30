
module Transform.Normalise(normaliseIHite, normaliseIFunc) where

import Transform.Type
import General.General


normaliseIHite :: IHite -> IHite
normaliseIHite (IHite datas funcs) = IHite datas (map normaliseIFunc funcs)


normaliseIFunc :: IFunc -> IFunc
normaliseIFunc (Func name args body) =
		Func name newargs $ normaliseIExpr newfree (zip args newargs) body
	where
		newargs = [0..newfree-1]
		newfree = length args


normaliseIExpr :: Int -> [(Int,Int)] -> IExpr -> IExpr
normaliseIExpr free rep x =
	case x of
		Var i -> Var $ lookupJust i rep
		Make x xs -> Make x (fs xs)
		Call x xs -> Call x (fs xs)
		Apply x xs -> Apply (f x) (fs xs)
		Sel x y -> Sel (f x) y
		Lambda is x -> Lambda newis $ normaliseIExpr newfree (zip is newis ++ rep) x
			where
				newis = [free..newfree-1]
				newfree = free + length is

		x -> x
	where
		f = normaliseIExpr free rep
		fs = map f

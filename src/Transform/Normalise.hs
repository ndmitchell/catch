
module Transform.Normalise(normaliseIHite, normaliseIFunc) where

import Transform.Type
import General.General
import Safe


normaliseIHite :: IHite -> IHite
normaliseIHite (IHite datas funcs) = IHite datas (map normaliseIFunc funcs)


normaliseIFunc :: IFunc -> IFunc
normaliseIFunc (Func name args body tweaks) =
		Func name newargs (normaliseIExpr newfree (zip args newargs) body) tweaks
	where
		newargs = [0..newfree-1]
		newfree = length args


normaliseIExpr :: Int -> [(Int,Int)] -> IExpr -> IExpr
normaliseIExpr free rep x =
	case x of
		Var i -> Var $ lookupJustNote ("Transform.Normalise " ++ show (free,rep,x)) i rep
		Lambda is x -> Lambda newis $ normaliseIExpr newfree (zip is newis ++ rep) x
			where
				newis = [free..newfree-1]
				newfree = free + length is

		_ -> setChildren x (map (normaliseIExpr free rep) (getChildren x))

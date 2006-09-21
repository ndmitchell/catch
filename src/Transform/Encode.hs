
module Transform.Encode(encode) where

import Hite.Type as L
import Transform.Type as R
import General.General


encode :: Hite -> IHite
encode hite@(Hite datas funcs) = IHite datas (map (enFunc hite) funcs)


enFunc :: Hite -> Func -> IFunc
enFunc hite (L.Func name args body _) = R.Func name newargs (enExpr hite nargs (zip args newargs) body) []
	where
		nargs = length args
		newargs = [0..nargs-1]


enExpr :: Hite -> Int -> [(String,Int)] -> Expr -> IExpr
enExpr hite free rep x = case x of
        L.Make x xs -> R.Make x (fs xs)
        L.Prim x xs -> R.Prim x (fs xs)
        L.Var x -> R.Var $ lookupJust x rep
        L.Sel x y -> R.Sel (f x) y
        L.Case x y -> R.Case (f x) [(a,f b) | (a,b) <- y]
        L.Call x xs -> R.Apply (f x) (fs xs)
        L.Error x -> R.Error x
        L.CallFunc x -> R.Lambda args (R.Call x (map R.Var args))
            where
                arity = length $ L.funcArgs $ L.getFunc hite x
                args = [free .. arity+free-1]
        _ -> error $ "Transform.Encode.enExpr: " -- ++ show x
    where
        f = enExpr hite free rep
        fs = map f


module Transform.Decode(decode) where

import Hite.Type as R
import Transform.Type as L
import General.General

decode :: IHite -> Hite
decode (IHite datas funcs) = Hite datas (map deFunc funcs)


deFunc :: IFunc -> Func
deFunc (L.Func name args body _) = R.Func name (map deVar args) (deExpr body) ""


deExpr :: IExpr -> Expr
deExpr x = case x of
        L.Make x xs -> R.Make x (fs xs)
        L.Prim x xs -> R.Prim x (fs xs)
        L.Var x -> R.Var $ deVar x
        L.Sel x y -> R.Sel (f x) y
        L.Case x y -> R.Case (f x) [(a,f b) | (a,b) <- y]
        L.Call x xs -> R.Call (R.CallFunc x) (fs xs)
        L.Error x -> R.Error x
        L.Unknown -> R.Unknown
        _ -> error $ "Transform.Decode.deExpr: " ++ show x
    where
        f = deExpr
        fs = map f


deVar :: Int -> String
deVar x = "v" ++ show x

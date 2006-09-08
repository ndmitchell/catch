
module Transform.Decode(decode) where

import Hite.Type as R
import Transform.Type as L
import General.General

decode :: IHite -> Hite
decode ihite@(IHite datas funcs) = Hite datas (map (deFunc ihite) funcs)


deFunc :: IHite -> IFunc -> Func
deFunc ihite (L.Func name args body _) = R.Func name (map deVar args) (deExpr ihite body) ""


deExpr :: IHite -> IExpr -> Expr
deExpr ihite x = case x of
        L.Make x xs -> R.Make x (fs xs)
        L.Prim x xs -> R.Prim x (fs xs)
        L.Var x -> R.Var $ deVar x
        L.Sel x y -> R.Sel (f x) y
        L.Case x y -> R.Case (f x) [(a,f b) | (a,b) <- y]
        L.Call x xs -> R.Call (R.CallFunc x) (fs xs)
        L.Error x -> R.Error x
        L.Unknown -> R.Unknown
        _ -> error $ "Transform.Decode.deExpr: " ++ show x ++ "\n\n" ++ output ihite
    where
        f = deExpr ihite
        fs = map f


deVar :: Int -> String
deVar x = "v" ++ show x

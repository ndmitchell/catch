
module Transform.EncodeSpec(encodeSpec) where

import Transform.Type
import General.General


encodeSpec :: IHite -> IHite
encodeSpec ihite@(IHite a b) = IHite a (concatMap (encodeFunc ihite) b)


encodeFunc :: IHite -> IFunc -> [IFunc]
encodeFunc ihite (Func name args body _)
        = [Func name args (mapOverOld f body) [(TweakExpr (map (const $ Var 0) args), name)]]
    where
        f (Call x xs) = Cell (FuncPtr x (map (const $ Var 0) $ funcArgs $ getFunc ihite x)) 0 xs
    
        f (Lambda [] x) = x
        f (Lambda as (Cell x n bs@(_:_)))
            | Var (last as) == last bs
            = f $ Lambda (init as) (Cell x (n+1) (init bs))
        
        f x = x
        

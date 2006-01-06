
module Hite.Kind(kind) where

import Hite.Type
import Maybe


kind :: Hite -> Hite
kind (Hite datas funcs) = Hite datas (minKindFix $ map initial funcs)
    where
        initial (Func name args body _) =
            Func name args body (Arrow (replicate (1+length args) Star))


minKindFix :: [Func] -> [Func]
minKindFix xs = if sameKinds then xs2 else minKindFix xs3
    where
        xs2 = map (minKindVar (map g xs)) xs
        items = concatMap (minKindFunc (map g xs2)) xs2
        
        sameKinds = and $ zipWith (==) (map funcKind xs) (map funcKind xs3)
        
        xs3 = map f xs2
        f func@(Func name _ _ k) = func{funcKind =
            foldr mgu k [b | (a,b) <- items, a == name]}
            
        g func = (funcName func, funcKind func)


minKindFunc :: [(FuncName, Kind)] -> Func -> [(FuncName, Kind)]
minKindFunc kinds func = applyKinds kinds func isCallFunc


-- find the minimum kind of each variable
minKindVar :: [(FuncName, Kind)] -> Func -> Func
minKindVar kinds func@(Func _ []   _ _ ) = func
minKindVar kinds func@(Func _ args _ (Arrow k1)) = func{funcKind=k2}
    where
        k2 = Arrow $ zipWith g args k1 ++ drop (length args) k1
        g arg k = foldr mgu k [b | (a,b) <- items, a == arg]
        items = applyKinds kinds func isVar
            

applyKinds :: [(FuncName, Kind)] -> Func -> (Expr -> Bool) -> [(String, Kind)]
applyKinds kinds func check =
        filter ((/=) Star . snd) $
        concatMap (f Star) $ allExpr func
    where
        f k (Call a b) = f k2 a ++ concat (zipWith f arr b)
            where
                arr = fromArrow k2
                k2 = mgu (g a) (Arrow (replicate (1+length b) Star))
            
        f k (Sel  a b) = f (kinded b k) a
        f k x@(Var a b) | check x = [(a, k)]
        f k x@(CallFunc a) | check x = [(a, k)]
        f k _ = []
        
        g (CallFunc x) = fromJust $ lookup x kinds
        g (Var x _) = head [a | (a,b) <- zip (fromArrow (funcKind func)) (funcArgs func), b == x]
        g x = Star
                

kinded sel Star = Star
kinded sel (Kinded [(s,k)]) = Kinded [(sel:s,k)]
kinded sel k = Kinded [([sel],k)]

arrow [x] = x
arrow xs  = Arrow xs

fromArrow (Arrow x) = x
fromArrow x = [x]


mgu :: Kind -> Kind -> Kind
mgu (Arrow as) (Arrow bs) = arrow $
        (take lmin (zipWith mgu as bs)) ++
        (fromArrow $ mgu (arrow (drop lmin as)) (arrow (drop lmin bs)))
    where lmin = min (length as) (length bs) - 1

mgu Star x@(Arrow _) = x
mgu x@(Arrow _) Star = x
mgu Star Star = Star
mgu a b = error $ "mgu not written: " ++ show (a,b)


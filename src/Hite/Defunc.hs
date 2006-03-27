
module Hite.Defunc(cmd, defunc) where

import Hite.Type
import Hite.Normalise

import List
import Maybe

cmd = cmdHitePure (const defunc) "defunc"
            "Perform Reynold's style defunctionalisation"


defunc :: Hite -> Hite
defunc bad_hite = Hite (newData ++ datas) (newFuncs ++ oldFuncs)
    where
        newData = if null items then [] else [generateData hite items]
        oldFuncs = mapExpr f funcs
        items = collectArities hite
        hite@(Hite datas funcs) = arityRaise (normalise bad_hite)

        f (Call (CallFunc x) xs) | lxs < args
                = Make ("%Ap_" ++ x ++ "_" ++ show lxs) xs
            where
                args = length $ funcArgs $ getFunc hite x
                lxs = length xs
        f x@(Call (CallFunc _) _) = x
        f (Call x xs) = Call (CallFunc $ "%ap" ++ show (length xs)) (x:xs)
        f x = x
        
        
        reqFuncs = sort $ nub [read xs :: Int | CallFunc ('%':'a':'p':xs) <- allExpr oldFuncs]
        newFuncs = map g reqFuncs
        
        g n = Func ("%ap" ++ show n) ("f":args) (Case (Var "f" "") (concatMap f items)) ""
            where
                args = map (('x':) . show) [1..n]
                
                f (name,arity) | diff >= n = [(nam,
                                   (if diff == n then Call (CallFunc name) else Make ("%Ap_" ++ name ++ "_" ++ show (arity+n)))
                                   (map (\x -> Sel (Var "f" "") (nam ++ "_" ++ show x)) [1..arity] ++ map (`Var` "") args)
                                   )]
                    where
                        nam = "%Ap_" ++ name ++ "_" ++ show arity
                        real = length $ funcArgs $ getFunc hite name
                        diff = real - arity
                f _ = []
        


generateData :: Hite -> [(FuncName, Int)] -> Data
generateData hite xs = Data "%Ap" (map f xs)
    where
        f (name, size) = Ctor t [t ++ "_" ++ show i | i <- [1..size]]
            where t = "%Ap_" ++ name ++ "_" ++ show size



-- find out which arities need generating
collectArities :: Hite -> [(FuncName, Int)]
collectArities hite = nub $ concatMap f items
    where
        items = nub $ [(x, length xs) | Call (CallFunc x) xs <- allExpr hite]
        funs = [(x,length xs) | Func x xs _ _ <- funcs hite]
        
        f (name, args) = [(name, x) | x <- [args..i-1]]
            where i = fromJust $ lookup name funs




-- never Call (CallFunc x) args
-- with more args than x expects
-- can decurry these instances
arityRaise :: Hite -> Hite
arityRaise hite = mapExpr f hite
    where
        f (Call (CallFunc x) xs) | length xs > args
                = Call (Call (CallFunc x) (take args xs)) (drop args xs)
            where args = length $ funcArgs $ getFunc hite x
        f x = x



{-
VERSION 1:
Total defunctionalisation: good
defunc's things that don't need to be: bad

defunc :: Hite -> Hite
defunc bad_hite = Hite (newData:datas) (newFuncs ++ mapExpr f funcs)
    where
        hite@(Hite datas funcs) = normalise bad_hite
        (newData, newFuncs) = newItems hite
        
        f (Call x [] ) = x
        f (Call x xs ) = Call (CallFunc ("^ap" ++ show (length xs))) (x:xs)
        f (CallFunc x) = Make ("^Ap_" ++ x) []
        f x = x



newItems :: Hite -> (Data, [Func])
newItems (Hite datas funcs) = (newData, newFunc : baseFunc : map stdFunc [2..mx+1])
    where
        newData = Data "^Ap" (map f items)
        newFunc = Func "^ap" ["x","y"] (Case (Var "x" "") (map g items)) Star
        
        mx = maximum [c | (a,b,c,d) <- items]
    
        items = [("^Ap_" ++ name ++ (if a == 0 then "" else "_" ++ show a), name, a, a == length args-1) |
                 Func name args body _ <- funcs, a <- [0..length args-1]]
        
        f (nam, name, args, final) = Ctor nam (sels nam args)
            
        g (nam, name, args, final) = (nam, (if final then Call (CallFunc name) else Make next) params)
            where
                params = map (Var "x" "" `Sel`) (sels nam args) ++ [Var "y" ""]
                next = "^Ap_" ++ name ++ "_" ++ show (args+1)
        
        sels nam args = [tail nam ++ "_" ++ show a | a <- [1..args]]


        baseFunc = Func "^ap1" ["x1","x2"] (Call (CallFunc "^ap") [Var "x1" "",Var "x2" ""]) Star
        stdFunc i = Func ("^ap" ++ show i) args
                (Call (CallFunc ("^ap" ++ show (i-1))) (Call (CallFunc "^ap") (take 2 params) : (drop 2 params)))
                Star
            where
                args = ['x' : show n | n <- [1..i+1]]
                params = map (`Var` "") args
-}

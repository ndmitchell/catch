
module Hite.Defunc(defunc) where

import Hite.Type
import Hite.Normalise


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

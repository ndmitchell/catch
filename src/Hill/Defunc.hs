
module Hill.Defunc(defunc, cmdsDefunc) where

import Hill.Type
import Hill.Simple
import General.General


cmdsDefunc = [hillCmdPure "defunc" (const defunc)]


---------------------------------------------------------------------

defunc :: Hill -> Hill
defunc hill = Hill (applyData : datas2) (applyFunc : funcs2)
    where
        Hill datas2 funcs2 = mapOverHill f $ applyFuns hill 
    
        f (Apply (Fun x) xs) = Make (x ++ "%" ++ show (length xs)) xs
        f (Apply x xs) = foldl (\a b -> Call "ap%" [a,b]) x xs
        f x = x
        

        applyUsers :: [(FuncName, Int)]        
        applyUsers = map split $ snub [x | Make x _ <- allOverHill funcs2, '%' `elem` x]
            where split x = let (a,_:b) = break (== '%') x in (a, read b)

        allApplys = snub $ concatMap f applyUsers
            where
                f (name,n) = [(name,i) | i <- [n..arity-1]]
                    where arity = length $ funcArgs $ getFunc hill name

        
        applyData = Data "Ap%" (map f allApplys) []
            where
                f (name,n) = Ctor nam [nam ++ "_" ++ show i | i <- [1..n]] []
                    where nam = name ++ "%" ++ show n
        
        applyFunc = Func "ap%" [0,1] (Case (Var 0) (map f allApplys))
            where
                f (name,n) = Alt (ACtor nam) body
                    where
                        body = if arity == n+1
                               then Call name args
                               else Make (name ++ "%" ++ show (n+1)) args
                        args = [Sel (Var 0) (nam ++ "_" ++ show i) | i <- [1..n]] ++ [Var 1]

                        arity = length $ funcArgs $ getFunc hill name
                        nam = name ++ "%" ++ show n

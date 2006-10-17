
module Hill.Defunc(defunc, cmdsDefunc) where

import Hill.Type
import Hill.Simple
import Data.List
import General.General


cmdsDefunc = [hillCmdPure "defunc" (const defunc)
             ,hillCmdPure "reach-defunc" (const reachDefunc)]


---------------------------------------------------------------------


splitName :: String -> (FuncName, Int)
splitName x = let (a,_:b) = break (== '%') x in (a, read b) 

joinName :: FuncName -> Int -> String
joinName a b = a ++ "%" ++ show b


isName :: String -> Bool
isName x = '%' `elem` x


defunc :: Hill -> Hill
defunc hill = Hill (applyData : datas2) (applyFunc : funcs2)
    where
        Hill datas2 funcs2 = mapOverHill f $ applyFuns hill 
    
        f (Apply (Fun x) xs) = Make (x ++ "%" ++ show (length xs)) xs
        f (Apply x xs) = foldl (\a b -> Call "ap%" [a,b]) x xs
        f x = x
        

        applyUsers :: [(FuncName, Int)]        
        applyUsers = map splitName $ snub [x | Make x _ <- allOverHill funcs2, isName x]

        allApplys = snub $ concatMap f applyUsers
            where
                f (name,n) = [(name,i) | i <- [n..arity-1]]
                    where arity = length $ funcArgs $ getFunc hill name

        
        applyData = Data "Ap%" (map f allApplys) []
            where
                f (name,n) = Ctor nam [nam ++ "_" ++ show i | i <- [1..n]] []
                    where nam = joinName name n
        
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


reachDefunc :: Hill -> Hill
reachDefunc hill | null keep = Hill restDatas restFuncs
                 | otherwise = Hill (newData:restDatas) (newFunc++restFuncs)
    where
        Data dnam ctrs typs = getData hill "Ap%"
        
        restDatas = [d | d <- datas hill, dataName d /= "Ap%"]
        (apFunc,restFuncs) = partition (\x -> funcName x == "ap%") (funcs hill)
        
        users = snub [splitName x | func <- funcs hill, funcName func /= "ap%",
                                    Make x _ <- allOverHill $ body func, isName x]
        
        lowball = map head $ groupBy (\a b -> fst a == fst b) users
        
        keep = filter f $ map (splitName . ctrName) ctrs
            where
                f (nam,n) = case lookup nam lowball of
                                Just i | i <= n -> True
                                _ -> False

        newData = Data dnam [c | c <- ctrs, splitName (ctrName c) `elem` keep] typs
        
        newFunc = case apFunc of
                      [] -> []
                      [Func fnam args (Case on alts)] ->
                            [Func fnam args $ Case on [alt | alt <- alts, let ACtor x = altVal alt,
                                                             splitName x `elem` keep]]

        ctrName (Ctor a _ _) = a
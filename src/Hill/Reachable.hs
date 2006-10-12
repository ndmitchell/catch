
module Hill.Reachable(reachable, cmdsReachable) where

import Hill.Type
import General.General


cmdsReachable = [hillCmdPure "reachable" reachable]



reachable :: FuncName -> Hill -> Hill
reachable ""   hill = reachableList ["main"] hill
reachable name hill = reachableList [name]   hill


reachableList :: [FuncName] -> Hill -> Hill
reachableList names hill@(Hill datas funcs) = Hill aliveDatas aliveFuncs
    where
        aliveFuncs = [x | x <- funcs, funcName x `elem` aliveFuncNames]
        aliveDatas = [x | x@(Data name _ _) <- datas, name `elem` aliveDataNames]
        
        aliveFuncNames = fixSet allFuncs names
        aliveDataNames = snub $ concatMap allDatas aliveFuncNames
        
        allFuncs = snub . concatMap (reachFuncs hill) . allOverHill . body . getFunc hill
        allDatas = snub . concatMap (reachDatas hill) . allOverHill . body . getFunc hill



reachFuncs :: Hill -> Expr -> [FuncName]
reachFuncs hill x = case x of
    Call x xs -> [x]
    Fun x -> [x]
    _ -> []


reachDatas :: Hill -> Expr -> [DataName]
reachDatas hill x =
    case x of
        Make x _ -> fromCtor x
        Const x -> fromConst x
        Sel _ x -> fromCArg x
        Case _ x -> concatMap fromAlt x
        _ -> []
    where
        fromAlt (Alt x _) = fromConst x
        fromAlt _ = []
        
        fromConst (ACtor x) = fromCtor x
        fromConst _ = []
        
        fromCArg x = [dataName $ getCArg hill x]
        fromCtor x = [dataName $ getCtor hill x]

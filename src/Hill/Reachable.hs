
module Hill.Reachable(reachable, cmdsReachable) where

import Hill.Type
import Hill.Producer
import General.General


cmdsReachable = [hillCmdPure "reachable" reachable]



reachable :: FuncName -> Hill -> Hill
reachable ""   hill = reachableList ["main"] hill
reachable name hill = reachableList [name]   hill


reachableList :: [FuncName] -> Hill -> Hill
reachableList names hill@(Hill datas funcs) = Hill aliveDatas aliveFuncs
    where
        -- function stuff
        aliveFuncs = fst $ producer hill (map (getFunc hill) names) processor generator
        
        processor :: Monad m => (FuncName -> m FuncName) -> Func -> m Func
        processor ask x = do bod <- mapOverOldM f $ body x ; return x{body = bod}
            where
                f (Call x xs) = do x2 <- ask x ; return $ Call x2 xs
                f (Fun x) = do x2 <- ask x ; return $ Fun x2
                f x = return x


        generator :: FuncName -> Int -> Func
        generator fun _ = getFunc hill fun

        -- data stuff
        aliveDatas = [x | x@(Data name _ _) <- datas, name `elem` aliveDataNames]
        aliveDataNames = snub $ concatMap allDatas aliveFuncs
        allDatas = snub . concatMap (reachDatas hill) . allOverHill . body



reachDatas :: Hill -> Expr -> [DataName]
reachDatas hill x =
    case x of
        Make x _ -> fromCtor x
        Ctr x -> fromCtor x
        Sel _ x -> fromCArg x
        Case _ x -> concatMap fromAlt x
        _ -> []
    where
        fromAlt (AltCtr x _) = fromCtor x
        fromAlt _ = []
        
        fromCArg x = [dataName $ getCArg hill x]
        fromCtor x = [dataName $ getCtor hill x]

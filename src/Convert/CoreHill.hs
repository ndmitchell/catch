
module Convert.CoreHill(convHill, mergeHills, convDatas, convFuncs) where

import Core
import Hill.All
import Convert.CoreData

import List
import Maybe
import Char
import General.General


mergeHills :: [Hill] -> Hill
mergeHills xs = reachable "" $
                Hill (concatMap datas xs) (concatMap funcs xs)


convHill :: Core -> Hill
convHill (Core n d datas funcs) = Hill newData (map (convFunc newData) funcs)
    where newData = map convData datas


convDatas :: [CoreData] -> Hill
convDatas xs = Hill (map convData xs) []


convFuncs :: Hill -> [CoreFunc] -> Hill
convFuncs hill xs = Hill [] (map (convFunc (datas hill)) xs)


getName (CoreVar x) = x
getName x = error $ "Convert.CoreHite.getName: pattern match failure, " ++ show x


convFunc :: [Data] -> CoreFunc -> Func
convFunc datas (CoreFunc name args body) = 
        Func name newargs (if isPrimitive res then Prim name (map Var newargs) else res)
    where
        res = convExpr datas name (zip args $ map Var newargs) nargs body
        
        isPrimitive (Apply x []) = isPrimitive x
        isPrimitive (Fun "primitive") = True
        isPrimitive _ = False
    
        newargs = [0..nargs-1]
        nargs = length args


convExpr :: [Data] -> FuncName -> [(String, Expr)] -> Int -> CoreExpr -> Expr
convExpr datas name ren free x = 
    case x of
        -- do the simple ones first
        CorePos _ x -> f x
        
        CoreApp x xs -> Apply (f x) (map f xs)
        
        CoreVar x -> case lookup x ren of
                          Just y -> y
                          Nothing -> Fun x
        
        CoreCase on opts -> Let [(free, f on)] $ Case (Var free) $ map (uncurry g) opts
            where
                g (CoreVar "_") y = Default $ f2 ren (free+1) y
                g (CoreApp (CoreCon x) xs) y = Alt (ACtor x) $ f2 (newren++ren) (free+1) y
                    where
                        newren = zipWith (\a b -> (getName a, Sel (Var free) b)) xs sels
                        sels = ctorArgs $ getCtor (Hill datas []) x
                
                g (CoreCon x) y = g (CoreApp (CoreCon x) []) y
                
                g x y = Alt (convConst x) $ f2 ren (free+1) y
        
        CoreLet [] body -> f body
        CoreLet binds body ->
                if null topbinds then
                    error $ "Convert.CoreHite, mutually recursive let in " ++ name
                else
                    Let (zip [free..] (map (f . snd) topbinds)) $
                    f2 (newren++ren) (free+ntopbinds) (CoreLet botbinds body)
            where
                bindnames = map fst binds
                (topbinds, botbinds) = partition isTopLevel binds
                ntopbinds = length topbinds
                newren = zip bindnames $ map Var [free..]
                isTopLevel (_, body) = null [x | CoreVar x <- allCore body, x `elem` bindnames]
            

        _ -> Const $ convConst x
    
    where
        f2 = convExpr datas name -- can't change
        f = f2 ren free
        
        
        
        convConst x = case x of
            CoreCon x -> ACtor x
            CoreInt x -> AInt x
            CoreInteger x -> AInteger x
            CoreDouble x -> ADouble x
            CoreFloat x -> AFloat x
            CoreChr x -> AChar x
            CoreStr x -> AString x

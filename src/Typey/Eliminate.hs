
module Typey.Eliminate(eliminate) where

import Typey.Type
import Typey.Subtype
import Hite
import General.General
import Data.Maybe
import Data.List
import Data.Char
import Data.Predicate
import Debug.Trace


eliminate :: Hite -> DataM SmallT -> TypeList -> TypeList -> TypeList
eliminate hite datam datat funct =
        if typeListLen funct == typeListLen funct2
        then funct
        else eliminate hite datam datat funct2
    where
        typeListLen x = length $ concatMap snd x
    
        funct2 = map f funct
        f (name, typs) = (name, filter (validType (hite,datam,datat,funct) name) typs)


type Env = (Hite, DataM SmallT, TypeList, TypeList)

-- given a datat and a funct, check that the function can have this type
validType :: Env -> String -> TSubtype -> Bool
validType env@(hite,datam,datat,funct) funcname (TArr argt res) =
        if isTBot res then not $ null resb
        else if null resn then False
        else res `isTSubset` unionList resn
    where
        (resb,resn) = partition isTBot ress
        ress = concat [getType env rep e | MCaseAlt p e <- opts, doesMatch env rep p]
        rep = zip args argt
        Func _ args (MCase opts) _ = getFunc hite funcname


getTypeRec :: Env -> [(FuncArg, TSubtype)] -> Expr -> [TSubtype]
getTypeRec env args expr = trace (show ("getTypeRec",args,expr,ans)) ans
    where ans = getType env args expr


-- get the type of an expression in an environment
getType :: Env -> [(FuncArg, TSubtype)] -> Expr -> [TSubtype]
getType env@(hite,datam,datat,funct) args expr =
    case expr of
        Call x xs -> getTCall (getT x) xs
        Make x xs -> getTCall (lookupJust x datat) xs
        CallFunc name -> lookupJust name funct
        Var x -> [lookupJust x args]
        Sel x path -> map (`getTSel` path) (getT x)
        Error _ -> [TBot]
        
        _ -> error $ show ("getType",args,expr)
    where
        getT = getType env args
        
        getTCall x [] = x
        getTCall x (y:ys) = getTCall (apply x (getT y)) ys

        getTSel (TBind (TPair _ x:xs)) path =
            case argElem of
                Self -> TBind [head xs, head xs]
                FreeS i -> x !! i
            where
                argElem = args2 !! (fromJust $ elemIndex path args) 
                Ctor name args = getCtorFromArg hite path
                args2 = head [args | DataT _ cs <- map snd datam, CtorT nam args <- cs, nam == name]
                
        
        apply :: [TSubtype] -> [TSubtype] -> [TSubtype]
        apply xs ys = {- trace (show ("apply",xs,ys,res)) -} res
            where
                res = concat [f x y | x <- xs, y <- ys]
                f _ TBot = [TBot]
                f (TArr (x:xs) y) z | x `isTSubset` z = [tArr (map uni xs) (uni y)]
                    where uni = applyUnify (unify x z)
                f _ _ = []

applyUnify :: [(String, TSubtype)] -> TSubtype -> TSubtype
applyUnify dat (TBind xs) = TBind $ map f xs
    where
        f (TPair a1 b1) = TPair a1 (map (applyUnify dat) b1)

applyUnify dat (TFree []) = TFree []
applyUnify dat o@(TFree [n]) = case lookup n dat of
                                          Nothing -> o
                                          Just x -> x
applyUnify dat (TArr x y) = TArr (map (applyUnify dat) x) (applyUnify dat y)
applyUnify dat TBot = TBot
applyUnify dat x = error $ show ("applyUnify",dat,x)



-- x `isTSubset` y
-- figure what a variable in x would have to be mapped to
unify :: TSubtype -> TSubtype -> [(String, TSubtype)]
unify (TBind xs) (TBind ys) = concat $ zipWith f xs ys
    where
        f (TPair a1 b1) (TPair a2 b2) = concat $ zipWith unify b1 b2
unify (TFree []) _ = []
unify (TFree [a]) x = [(a,x)]

unify x y = error $ show ("unify",x,y)


doesMatch :: Env -> [(FuncArg, TSubtype)] -> Pred MCaseOpt -> Bool
doesMatch env args p = mapPredBool f p
    where
        f (MCaseOpt e c) = c `elem` a
            where TBind (TPair a _:_) = unionList $ getType env args e

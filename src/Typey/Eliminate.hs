
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
import Control.Monad


type Logger = String -> IO ()
type Env = (Hite, DataM SmallT, TypeList, TypeList)

eliminate :: Logger -> Hite -> DataM SmallT -> TypeList -> TypeList -> IO TypeList
eliminate logger hite datam datat funct = do
        logger "== ELIMINATE\n"
        res <- newTypeList logger (hite,datam,datat,funct)
        case res of
            Nothing -> return funct
            Just x -> eliminate logger hite datam datat x



-- return Nothing if this is a fixed point
-- otherwise return Just the new result
newTypeList :: Logger -> Env -> IO (Maybe TypeList)
newTypeList logger env@(hite,datam,datat,funct) = 
    do
        br <- mapM f funct
        return $ if any fst br then Just $ map snd br else Nothing
    where
        f (name, opts) = do
            br <- mapM (g name) opts
            return (any fst br, (name, map snd br))
        
        g name (args,res) = do
            logger $ name ++ " :: " ++ show (TArr args res)
            let res2 = getFuncType env name args
                res3 = res2 -- unionPair res2 res
                same = res == res3
            logger $ if same then " KEEP\n" else " ===> " ++ show res3 ++ "\n"
            return (not same, (args,res3))


{-
-- given a datat and a funct, check that the function can have this type
validType :: Logger -> Env -> String -> TSubtype -> IO Bool
validType logger env@(hite,datam,datat,funct) funcname (TArr argt res) =
        -- logger (" $ " ++ show (resb,resn) ++ " $") >>
        (
        if isTBot res then return $ not $ null resb
        else if null resn then return False
        else return $ res `isTSubset` unionList resn
        )
    where
        (resb,resn) = partition isTBot ress

validType logger env funcname res = validType logger env funcname (TArr [] res)
-}

getFuncType :: Env -> FuncName -> [TSubtype] -> TSubtype
getFuncType env@(hite,datam,datat,funct) funcname argt = unionList ress
    where
        ress = concat [getType env rep e | MCaseAlt p e <- opts, doesMatch env rep p]
        rep = zip args argt
        Func _ args (MCase opts) _ = getFunc hite funcname



getTypeRec :: Env -> [(FuncArg, TSubtype)] -> Expr -> [TSubtype]
getTypeRec env args expr = trace (show ("getTypeRec",args,expr,ans)) ans
    where ans = getType env args expr


-- get the type of an expression in an environment
getType :: Env -> [(FuncArg, TSubtype)] -> Expr -> [TSubtype]
getType env@(hite,datam,datat,funct) args expr = traceNone ("getType " ++ show args ++ ", " ++ output expr) $
    case expr of
        Call x xs -> getTCall (getT x) xs
        Make x xs -> getTCall (liftTypeList $ lookupJust x datat) xs
        CallFunc name -> liftTypeList $ lookupJust name funct
        Var x -> [lookupJust x args]
        Sel x path -> concatMap (`getTSel` path) (getT x)
        Error _ -> [TBot]
        
        _ -> error $ show ("getType",args,expr)
    where
        liftTypeList x = [tArr a b | (a,b) <- x]
    
        getT = getType env args
        
        getTCall x [] = x
        getTCall x (y:ys) = getTCall (apply x (getT y)) ys

        getTSel :: TSubtype -> String -> [TSubtype]
        getTSel (TBind (TPair _ x:xs)) path =
            case argElem of
                Self -> if null xs then [] else [TBind [head xs, head xs]]
                FreeS i -> [x !! i]
            where
                argElem = args2 !! (fromJust $ elemIndex path args) 
                Ctor name args = getCtorFromArg hite path
                args2 = head [args | DataT _ cs <- map snd datam, CtorT nam args <- cs, nam == name]
                
        
        apply :: [TSubtype] -> [TSubtype] -> [TSubtype]
        apply xs ys = traceNone ("apply " ++ show xs ++ " AND " ++ show ys ++ " IS ") res
            where
                res = concat [f x y | x <- xs, y <- ys]
                f _ TBot = [TBot]
                f TBot _ = [TBot]
                f (TArr (x:xs) y) z | isJust mapping = [tArr (map uni xs) (uni y)]
                    where
                        uni = applyUnify $ fromJust mapping
                        mapping = unify x z
                f _ _ = []

applyUnify :: [(String, TSubtype)] -> TSubtype -> TSubtype
applyUnify dat (TBind xs) = TBind $ map f xs
    where
        f (TPair a1 b1) = TPair a1 (map (applyUnify dat) b1)

applyUnify dat (TFree []) = TFree []
applyUnify dat o@(TFree [n]) = case lookup n dat of
                                          Nothing -> o
                                          Just x -> x
applyUnify dat (TFree ns) = unionList $ map (applyUnify dat . TFree . box) ns
applyUnify dat (TArr x y) = tArr (map (applyUnify dat) x) (applyUnify dat y)
applyUnify dat TBot = TBot
applyUnify dat x = error $ show ("applyUnify",dat,x)



-- roughly x `isTSubset` y (for constructors at least)
-- figure what a variable in x would have to be mapped to
unify :: TSubtype -> TSubtype -> Maybe [(String, TSubtype)]
unify (TBind (x:xs)) (TBind (y:ys)) = liftM concat $ sequence $ f x y : zipWith g xs ys
    where
        f (TPair a1 b1) (TPair a2 b2) =
            if null $ a1 \\ a2
            then liftM concat $ sequence $ zipWith unify b1 b2
            else Nothing

        -- demand equality for child types, because of the powerset rule
        -- kinda hacky, needs formalising
        g p1@(TPair a1 b1) p2@(TPair a2 b2) =
            if a1 `setEq` a2 then f p1 p2 else Nothing
            
unify (TFree []) _ = Just []
unify _ (TFree []) = Just []
unify (TFree [a]) x = Just [(a,x)]

unify x y = error $ show ("unify",x,y)


doesMatch :: Env -> [(FuncArg, TSubtype)] -> Pred MCaseOpt -> Bool
doesMatch env args p = mapPredBool f p
    where
        f (MCaseOpt e c) = not (null res) && (c `elem` a)
            where
                TBind (TPair a _:_) = unionList res
                res = getType env args e

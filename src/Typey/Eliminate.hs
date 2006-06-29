
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
        (b,r) <- liftList f funct
        return $ if b then Just r else Nothing
    where
        liftList f xs = do
            br <- mapM f xs
            return (any fst br, map snd br)
    
        f :: (String, TSubtype) -> IO (Bool, (String, TSubtype))
        f (name,typ) = do
            (b,r) <- liftList (g name) (getTArrs typ)
            return (b,(name,tFunc r))
        
        g :: String -> TArr -> IO (Bool, TArr)
        g name t@(TArr args res) = do
            logger $ name ++ " :: " ++ show t
            let res2 = getFuncType env name args
                res3 = res2 -- unionPair res2 res
                same = res == res3
            logger $ if same then " KEEP\n" else " ===> " ++ show res3 ++ "\n"
            return (not same, TArr args res3)


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
        ress = [getType env rep e | MCaseAlt p e <- opts, doesMatch env rep p]
        rep = zip args argt
        Func _ args (MCase opts) _ = getFunc hite funcname



getTypeRec :: Env -> [(FuncArg, TSubtype)] -> Expr -> TSubtype
getTypeRec env args expr = trace (show ("getTypeRec",args,expr,ans)) ans
    where ans = getType env args expr


-- get the type of an expression in an environment
getType :: Env -> [(FuncArg, TSubtype)] -> Expr -> TSubtype
getType env@(hite,datam,datat,funct) args expr = traceNone ("getType " ++ show args ++ ", " ++ output expr) $
    case expr of
        Call x xs -> getTCall (getT x) xs
        Make x xs -> getTCall (lookupJust x datat) xs
        CallFunc name -> lookupJust name funct
        Var x -> lookupJust x args
        Sel x path -> getTSel (getT x) path
        Error _ -> TBot
        
        _ -> error $ show ("getType",args,expr)
    where
        getT = getType env args
        
        getTCall x [] = x
        getTCall x (y:ys) = getTCall (apply x (getT y)) ys

        getTSel :: TSubtype -> String -> TSubtype
        getTSel (TBind (TPair _ x:xs)) path =
            case argElem of
                Self -> if null xs then TFree [] else TBind [head xs, head xs]
                FreeS i -> x !! i
            where
                argElem = args2 !! (fromJust $ elemIndex path args) 
                Ctor name args = getCtorFromArg hite path
                args2 = head [args | DataT _ cs <- map snd datam, CtorT nam args <- cs, nam == name]
                
        
        apply :: TSubtype -> TSubtype -> TSubtype
        apply func args = f func args
            where
                f _ TBot = TBot
                f TBot _ = TBot
                f (TFunc xs) y = tFunc $ nub $ concatMap (`g` y) xs
                f _ _ = TFree []

                g (TArr (x:xs) y) z | isJust mapping = [TArr (map uni xs) (uni y)]
                    where
                        uni = applyUnify $ fromJust mapping
                        mapping = unify x z
                g _ _ = []


applyUnify :: [(String, TSubtype)] -> TSubtype -> TSubtype
applyUnify dat (TBind xs) = TBind $ map f xs
    where
        f (TPair a1 b1) = TPair a1 (map (applyUnify dat) b1)

applyUnify dat (TFree []) = TFree []
applyUnify dat o@(TFree [n]) = case lookup n dat of
                                          Nothing -> o
                                          Just x -> x
applyUnify dat (TFree ns) = unionList $ map (applyUnify dat . TFree . box) ns
applyUnify dat (TFunc x) = tFunc $ map f x
    where f (TArr a b) = TArr (map (applyUnify dat) a) (applyUnify dat b)
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
unify (TFree []) TBot = Nothing
unify TBot (TFree []) = Nothing
unify TBot TBot = Just []
unify _ (TFree []) = Just []
unify (TFree [a]) x = Just [(a,x)]
unify TBot (TFree [a]) = Nothing
--unify (TArr a1 b1) (TArr a2 b2) =
--    liftM concat $ sequence $ zipWithEq unify (b1:a1) (b2:a2)


unify x y = error $ show ("unify",x,y)



doesMatch :: Env -> [(FuncArg, TSubtype)] -> Pred MCaseOpt -> Bool
doesMatch env args p = mapPredBool f p
    where
        f (MCaseOpt e c) = c `elem` a
            where
                TBind (TPair a _:_) = getType env args e

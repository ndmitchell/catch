
module Typey.Eliminate(eliminate) where

import Typey.Type
import Typey.Subtype
import Typey.Unify
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
        f [] funct
    where
        f done [] = return done
        f done todo = do
                res <- solveMany logger hite datam datat done this
                f (res++done) next
            where
                (this,next) = partition ((`elem` odeps) . fst) todo
                odeps = let (a,b) = minimumExtract (length . snd) deps in a:b
                deps = getDepsAll [(name, tdeps) | (name,_) <- todo,
                                   let tdeps = filter (`elem` todos) $ getDeps name]
                todos = map fst todo
    
        getDepsAll :: [(String, [String])] -> [(String, [String])]
        getDepsAll deps = [(a,fixSet (\x -> fromJust $ lookup x deps) b) | (a,b) <- deps]
        
        getDeps :: String -> [String]
        getDeps x = nub [y | CallFunc y <- allExpr $ getFunc hite x] \\ [x]



-- try solving those todo
solveMany :: Logger -> Hite -> DataM SmallT -> TypeList -> TypeList -> TypeList -> IO TypeList
solveMany logger hite datam datat done todo = do
        logger $ "== SOLVING: " ++ (intercat " " $ nub $ map fst todo) ++ "\n"
        f todo
    where
        f x = do
            logger $ "== solve:\n"
            res <- solveOnce logger (hite,datam,datat,done) x
            case res of
                Just x -> f x
                Nothing -> return x


-- solve a list one, return Nothing for done, Just x for the next version
solveOnce :: Logger -> Env -> TypeList -> IO (Maybe TypeList)
solveOnce logger env@(hite,datam,datat,funct) todo =
    do
        (b,r) <- liftList f todo
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
-}


getFuncType :: Env -> FuncName -> [TSubtype] -> TSubtype
getFuncType env@(hite,datam,datat,funct) funcname argt = unionList ress
    where
        ress = [getType env rep e | MCaseAlt p e <- opts, doesMatch env rep p]
        rep = zip args argt
        Func _ args (MCase opts) _ = getFunc hite funcname


{-
getTypeRec :: Env -> [(FuncArg, TSubtype)] -> Expr -> TSubtype
getTypeRec env args expr = trace (show ("getTypeRec",args,expr,ans)) ans
    where ans = getType env args expr
-}


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
                f (TFunc xs) y = combine $ concatMap (`g` y) xs
                f _ _ = TFree []
                g (TArr (x:xs) y) z = [TArr (map uni xs) (uni y) | m <- unify x z, let uni = applyUnify m]
                g _ _ = []
        
        combine :: [TArr] -> TSubtype
        combine xs = tFunc $ map g $ groupSetBy f xs
            where
                f (TArr a1 _) (TArr a2 _) = a1 == a2
                g xs@((TArr a r):_) = TArr a $ unionList [x | TArr _ x <- xs]



doesMatch :: Env -> [(FuncArg, TSubtype)] -> Pred MCaseOpt -> Bool
doesMatch env args p = mapPredBool f p
    where
        f (MCaseOpt e c) = c `elem` a
            where
                TBind (TPair a _:_) = getType env args e

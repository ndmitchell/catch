
module Typey.Eliminate(eliminate) where

import Typey.Type
import Typey.Subtype
import Typey.Operations
import Typey.Reason
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

eliminate :: FilePath -> Logger -> Hite -> DataM SmallT -> TypeList -> TypeList -> IO TypeList
eliminate logfile logger hite datam datat funct = do
        r <- reasonInit logfile
        logger "== ELIMINATE\n"
        res <- f r [] funct
        reasonTerm r
        return res
    where
        f r done [] = return done
        f r done todo = do
                res <- solveMany r logger hite datam datat done this
                f r (res++done) next
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
solveMany :: ReasonH -> Logger -> Hite -> DataM SmallT -> TypeList -> TypeList -> TypeList -> IO TypeList
solveMany r logger hite datam datat done todo = do
        logger $ "== SOLVING: " ++ msg ++ "\n"
        reasonSection r $ "Solving " ++ msg
        f bound todo
    where
        msg = intercat " " $ nub $ map fst todo
        
        -- a very generous bound, only for program error really
        bound = 1 + length todo + length (concat [x | (a, TFunc x) <- todo])
    
        f 0 orig = error $ "Non termination when solving: " ++ msg
        f (n+1) orig = do
            reasonSubsection r $ "Solve " ++ show (bound-n)
            when recursive $ logger $ "== solve " ++ show (bound-n) ++ ":\n"
            res <- solveOnce r logger (hite,datam,datat,done) orig
            case res of
                Just x | recursive -> f n x
                Just x | otherwise -> return x
                Nothing -> return orig

        recursive = any (`elem` funcs) [y | name <- funcs, CallFunc y <- allExpr $ getFunc hite name]
        funcs = map fst todo


-- solve a list one, return Nothing for done, Just x for the next version
solveOnce :: ReasonH -> Logger -> Env -> TypeList -> IO (Maybe TypeList)
solveOnce r logger env@(hite,datam,datat,funct) todo =
    do
        (b,r) <- liftList f todo
        return $ if b then Just r else Nothing
    where
        env2 = (hite,datam,datat,funct++todo)
    
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
            let res2 = getFuncType env2 name args
                res3 = reasonSubtype res2
                same = res == res3
                msg = if same then " KEEP" else " ===> " ++ show res3
            logger $ msg ++ "\n"
            reasonSubsubsection r $ name ++ " :: " ++ show t ++ msg
            reasonShow r $ res2
            return (not same, TArr args res3)


getFuncType :: Env -> FuncName -> [TSubtype] -> Reason
getFuncType env@(hite,datam,datat,funct) funcname argt =
        if null failed then 
            ReasonArgs rep (ReasonUnion res ress)
        else
            error $ "getFuncType-free, " ++
                    funcname ++ " :: " ++ intercat " -> " (map show argt) ++ "==" ++
                    show failed
    where
        failed = filter (not.isTBound) $ map reasonSubtype ress
    
        res = unionList $ map (tFree . reasonSubtype) ress
        ress = [getType env rep e | MCaseAlt p e <- opts, doesMatch env rep p]
        rep = zip args (map tBound argt)
        Func _ args (MCase opts) _ = getFunc hite funcname


-- get the type of an expression in an environment
getType :: Env -> [(FuncArg, TSubtype)] -> Expr -> Reason
getType env@(hite,datam,datat,funct) args expr =
    case expr of
        Call x xs -> getTCall (getT x) xs
        Make x xs -> getTCall (ReasonLookup (tFree $ lookupJust x datat) x) xs
        CallFunc name -> ReasonLookup (getTFunc name) name
        Var x -> ReasonLookup (lookupJust x args) x
        Sel x path -> getTSelR (getT x) path
        Error _ -> ReasonLookup TBot "error"
        
        _ -> error $ show ("getType") -- ,args,expr)
    where
        getT = getType env args
        
        getTFunc "_" = TAny
        getTFunc name = tFree $ lookupJust name funct
        
        getTCall x [] = x
        getTCall x (y:ys) = getTCall (applyR x (getT y)) ys

        getTSelR :: Reason -> String -> Reason
        getTSelR x path = ReasonFollow res path x
            where res = getTSel (reasonSubtype x) path

        getTSel :: TSubtype -> String -> TSubtype
        getTSel (TBind (TPair _ x:xs)) path =
            case argElem of
                Self -> if null xs then TVoid
                        else TBind (replicate (if isRec then 2 else 1) (head xs))
                FreeS i -> x !! i
            where
                isRec = any isSelf [a | (_,DataT _ ctr) <- datam, CtorT cn ars <- ctr, cn `elem` ctrs, a <- ars]
                TPair ctrs _ = head xs
            
                argElem = args2 !! (fromJust $ elemIndex path args) 
                Ctor name args _ = rawCtor $ getCArg hite path
                args2 = head [args | DataT _ cs <- map snd datam, CtorT nam args <- cs, nam == name]
                
        
        applyR :: Reason -> Reason -> Reason
        applyR lhs rhs = ReasonApply res lhs rhs
            where res = apply (reasonSubtype lhs) (reasonSubtype rhs)


doesMatch :: Env -> [(FuncArg, TSubtype)] -> Pred MCaseOpt -> Bool
doesMatch env args p = mapPredBool f p
    where
        f (MCaseOpt e c) = case reasonSubtype $ getType env args e of
                                TBind (TPair a _ : _) -> c `elem` a
                                _ -> False

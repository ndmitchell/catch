
module Typey.Solve2(typeySolve2) where

import Typey.Type
import IO
import Hite
import General.General
import Data.Maybe
import Data.List
import Data.Char
import Data.Predicate
import Debug.Trace


data TSubtype = TAtom [TAtom]
              | TBind [TPair]
              | TArr [TSubtype] TSubtype
              | TBot

data TPair = TPair [TAtom] [TSubtype]

data TAtom = TCtor String
           | TFree String
           deriving Eq

isTFree (TFree{}) = True; isTFree _ = False
isTBot (TBot{}) = True; isTBot _ = False

instance Show TSubtype where
    show (TAtom x) = show x
    show (TBind xs) = "{" ++ (concat $ intersperse " | " $ map show xs) ++ "}"
    show (TArr a b) = "(" ++ (concat $ intersperse " -> " $ map show (a++[b])) ++ ")"
    show TBot = "!"

instance Show TPair where
    show (TPair a b) = show a ++ " " ++ show b

instance Show TAtom where
    showList [] = showString "?"
    showList [x] = showString $ show x
    showList xs = showString $ "[" ++ (concat $ intersperse "," $ map show xs) ++ "]"
    
    show (TCtor x) = if x == "[]" then "#" else x
    show (TFree x) = x


instance Union TSubtype where
    unionPair (TAtom a) (TAtom b) = TAtom (a `union` b)
    unionPair (TBind a) (TBind b) = TBind (zipWithEq unionPair a b)
    unionPair (TArr a1 b1) (TArr a2 b2) = TArr (zipWithEq unionPair a1 a2) (b1 `unionPair` b2)
    unionPair a b = error $ show ("Union TSubtype",a,b)

instance Union TPair where
    unionPair (TPair a1 b1) (TPair a2 b2) = TPair (a1 `union` a2) (zipWithEq unionPair b1 b2)


isTSubset :: TSubtype -> TSubtype -> Bool
isTSubset (TBind xs) (TBind ys) | length xs > length ys = False
                                | otherwise = and $ zipWith isTSubsetPair xs ys
isTSubset (TAtom xs) (TAtom ys) = isTSubsetAtom xs ys
isTSubset TBot TBot = True
isTSubset TBot _ = False
isTSubset (TAtom [TFree _]) _ = True
isTSubset x y = error $ show ("isTSubset",x,y)


isTSubsetPair (TPair x1 y1) (TPair x2 y2) = isTSubsetAtom x1 x2 && and (zipWithEq isTSubset y1 y2)


isTSubsetAtom xs ys = null $ filter isTFree xs \\ filter isTFree ys



type TypeList = [(String, [TSubtype])]




typeySolve2 :: String -> Handle -> Hite -> DataM SmallT -> Func2M -> IO Bool
typeySolve2 file hndl hite datam funcm =
    do
        outBoth "-- TYPES OF FUNCTIONS"
        out $ unlines [a ++ " :: " ++ show b | (a,b) <- funcm]
        outBoth "-- SUBTYPES OF DATA"
        out $ showTypeList datat
        outBoth "-- SUBTYPES OF FUNCTIONS"
        out $ showTypeList funct
        outBoth "-- VALID SUBTYPES"
        out $ showTypeList funct2
        return False
    where
        datat = basicTypes datam
        funct = funcTypes datam funcm
        funct2 = prune hite datam datat funct
    
        out = hPutStrLn hndl
        outBoth x = putStrLn x >> out x


showTypeList :: TypeList -> String
showTypeList x = unlines $ concatMap f x
    where
        f (name,typs) = (name ++ " ::") : map show typs


-- get all the basic type information for constructors
basicTypes :: DataM SmallT -> TypeList
basicTypes datam = concatMap (dataSubtypes . snd) datam


dataSubtypes :: DataT SmallT -> TypeList
dataSubtypes (DataT n xs) = map f xs
    where
        getSelfs :: Char -> [TSubtype]
        getSelfs i = [TBind [TPair [a] (lst '1')] | a <- nas] ++
                     [TBind [TPair [a] (lst '1'), TPair [b] (lst '2')] | a <- ras, b <- nas ++ ras]
            where
                (ras,nas) = (\(a,b) -> (map snd a,map snd b)) $ partition fst
                    [(isRecursive ctr, TCtor q) | ctr@(CtorT q _) <- xs]
                
                lst j = take n [TAtom [TFree [c,i,j]] | c <- ['a'..'z']]
    
        f (CtorT name xs) = (,) name [TArr a (makeRes name (zip xs a)) | a <- args]
            where args = crossProduct $ zipWith g ['a'..] xs
        
        g i Self = getSelfs i
        g i (FreeS n) = [TAtom [TFree [i]]]

        makeRes :: String -> [(SmallT,TSubtype)] -> TSubtype
        makeRes name xs | not $ any (isSelf.fst) xs = TBind [TPair [TCtor name] vars]
                        | otherwise = TBind [TPair [TCtor name] vars, selfs]
            where
                selfs = unionListNote "dataSubtypes.selfs" [y | (Self,TBind x) <- xs, y <- x]
                vars = map h [0..n-1]
                h i = unionList (TAtom [] : [x | (FreeS j, x) <- xs, j == i])


funcTypes :: DataM SmallT -> Func2M -> TypeList
funcTypes datam funcm = [(name, getFuncSubtypes datam typ) | (name, typ) <- funcm]

-- get all the possible subtypes of a function
getFuncSubtypes :: DataM SmallT -> Large2T -> [TSubtype]
getFuncSubtypes datam = renameVars . addBottoms . getSubtypes datam


getSubtypes :: DataM SmallT -> Large2T -> [TSubtype]
getSubtypes datam (Free2T a) = [TAtom [TFree a]]
getSubtypes datam (Arr2T a b) = [TArr x y | x <- as, y <- getSubtypes datam b]
    where as = crossProduct $ map (getSubtypes datam) a
getSubtypes datam (Ctor2T a) = getSubtypes datam $ Bind2T (Ctor2T a) []

getSubtypes datam (Bind2T (Ctor2T x) xs) = map (simpSubtype datat) $
        [TBind [TPair [a] b] | a <- nas, b <- bs] ++
        [TBind [TPair [a1] b1, TPair [a2] b2] | a1 <- ras, b1 <- bs, a2 <- ras ++ nas, b2 <- bs]
    where
        datat = fromJust $ lookup x datam
        (ras,nas) = (\(a,b) -> (map snd a,map snd b)) $ partition fst
            [(isRecursive ctr, TCtor q) | let DataT _ y = datat, ctr@(CtorT q _) <- y]
        bs = crossProduct $ map (getSubtypes datam) xs


simpSubtype :: DataT SmallT -> TSubtype -> TSubtype
simpSubtype (DataT _ ctors) (TBind xs) = TBind (map f xs)
    where
        f (TPair [TCtor n] xs) = TPair [TCtor n] (zipWith f [0..] xs)
            where
                ctr = [y | CtorT n2 ys <- ctors, n2 == n, FreeS y <- ys]
                f i x = if i `elem` ctr then x else TAtom []



-- add bottom everywhere you can
addBottoms :: [TSubtype] -> [TSubtype]
addBottoms x = concat $ map f x
    where
        f (TArr as bs) = concat [[TArr a b, TArr a TBot] | a <- fs as, b <- f bs]
        f (TBind xs) = map TBind $ crossProduct $ map g xs
        f x = [x]
        
        fs xs = crossProduct $ map f xs
        
        g (TPair as bs) = [TPair as b | b <- fs bs]


-- rename all variables
renameVars :: [TSubtype] -> [TSubtype]
renameVars x = concatMap uniqueVars x



uniqueVars :: TSubtype -> [TSubtype]
uniqueVars (TArr arg res) = [TArr lhs b | b <- rhss]
    where
        lhs = insertAtoms arg $ f 0 $ extractAtoms arg
        lhsf = [x | [TFree x] <- extractAtoms lhs]
        
        rhs = extractAtoms [res]
        rhsf = [x | [TFree x] <- rhs]
        
        rhss = if null rhsf then [res]
               else concatMap (insertAtoms [res]) $ concatMap g (allItems rhs)
        
        g (pre,[TFree x],post) = [map blank pre ++ [[TFree y]] ++ map blank post | y <- h x]
        
        blank [TFree x] = []
        blank x = x

        h x = [y | y <- lhsf, takeWhile isAlpha y == x]
    
        f n ([TFree x]:xs) = [TFree (x ++ show n)] : f (n+1) xs
        f n (x:xs) = x : f n xs
        f n [] = []


extractAtoms :: [TSubtype] -> [[TAtom]]
extractAtoms x = concatMap fSubtype x
    where
        fSubtype (TAtom a) = [a]
        fSubtype (TBind a) = concatMap fPair a
        fSubtype (TArr a b) = concatMap fSubtype a ++ fSubtype b
        fSubtype (TBot) = []
        
        fPair (TPair a b) = [a] ++ concatMap fSubtype b


insertAtoms :: [TSubtype] -> [[TAtom]] -> [TSubtype]
insertAtoms x ns = fSubtypes x ns
    where
        fSubtypes :: [TSubtype] -> [[TAtom]] -> [TSubtype]
        fSubtypes [] [] = []
        fSubtypes (x:xs) ns = fSubtype x n1 : fSubtypes xs n2
            where (n1,n2) = splitAt (length $ extractAtoms [x]) ns
        fSubtypes x y = error $ show ("fSubtypes",x,y)

            
        fSubtype (TAtom a) [n] = TAtom n
        fSubtype (TBot) [] = TBot
        fSubtype (TArr a b) n = TArr (init ab) (last ab)
            where ab = fSubtypes (a ++ [b]) n
        fSubtype (TBind xs) n = TBind $ fPairs xs n

        fPairs [] [] = []
        fPairs (TPair a b : xs) (n:ns) = TPair n (fSubtypes b n1) : fPairs xs n2
            where (n1,n2) = splitAt (length $ extractAtoms b) ns

---------------------------------------------------------------------
-- PRUNE

prune :: Hite -> DataM SmallT -> TypeList -> TypeList -> TypeList
prune hite datam datat funct =
        if typeListLen funct == typeListLen funct2
        then funct
        else prune hite datam datat funct2
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
getTypeRec env args expr = error (show ("getTypeRec",args,expr,ans))
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
        apply xs ys = concat [f x y | x <- xs, y <- ys]
            where
                f _ TBot = [TBot]
                f (TArr (x:xs) y) z | x `isTSubset` z = [tArr (map uni xs) (uni y)]
                    where uni = applyUnify (unify x z)
                f _ _ = []

tArr [] y = y
tArr xs y = TArr xs y


applyUnify :: [(String, TSubtype)] -> TSubtype -> TSubtype
applyUnify dat (TBind xs) = TBind $ map f xs
    where
        f (TPair a1 b1) = TPair a1 (map (applyUnify dat) b1)

applyUnify dat (TAtom []) = TAtom []
applyUnify dat o@(TAtom [TFree n]) = case lookup n dat of
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
unify (TAtom []) _ = []
unify (TAtom [TFree a]) x = [(a,x)]

unify x y = error $ show ("unify",x,y)


doesMatch :: Env -> [(FuncArg, TSubtype)] -> Pred MCaseOpt -> Bool
doesMatch env args p = mapPredBool f p
    where
        f (MCaseOpt e c) = TCtor c `elem` a
            where TBind (TPair a _:_) = unionList $ getType env args e

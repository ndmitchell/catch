
module Typey.Solve2(typeySolve2) where

import Typey.Type
import IO
import Hite
import General.General
import Data.Maybe
import Data.List
import Data.Char


data TSubtype = TAtom [TAtom]
              | TBind [TPair]
              | TArr [TSubtype] TSubtype
              | TBot

data TPair = TPair [TAtom] [TSubtype]

data TAtom = TCtor String
           | TFree String
           deriving Eq


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


typeySolve2 :: String -> Handle -> Hite -> DataM SmallT -> Func2M -> IO Bool
typeySolve2 file hndl hite datam funcm =
    do
        outBoth "-- TYPES OF FUNCTIONS"
        out $ unlines [a ++ " :: " ++ show b | (a,b) <- funcm]
        outBoth "-- SUBTYPES OF DATA"
        out $ showTypeList datat
        outBoth "-- SUBTYPES OF FUNCTIONS"
        out $ showTypeList funct
        return False
    where
        datat = basicTypes datam
        funct = funcTypes datam funcm
    
        out = hPutStrLn hndl
        outBoth x = putStrLn x >> out x


showTypeList :: [(String, [TSubtype])] -> String
showTypeList x = unlines $ concatMap f x
    where
        f (name,typs) = (name ++ " ::") : map show typs


-- get all the basic type information for constructors
basicTypes :: DataM SmallT -> [(String, [TSubtype])]
basicTypes datam = concatMap (dataSubtypes . snd) datam


dataSubtypes :: DataT SmallT -> [(String, [TSubtype])]
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


funcTypes :: DataM SmallT -> Func2M -> [(String, [TSubtype])]
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

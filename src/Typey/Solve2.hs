
module Typey.Solve2(typeySolve2) where

import Typey.Type
import IO
import Hite
import General.General
import Data.Maybe
import Data.List


data TSubtype = TAtom [TAtom]
              | TBind [TPair]
              | TArr [TSubtype] TSubtype
              | TBot

instance Show TSubtype where
    show (TAtom x) = show x
    show (TBind xs) = "{" ++ (concat $ intersperse " | " $ map show xs) ++ "}"
    show (TArr a b) = "(" ++ (concat $ intersperse " -> " $ map show (a++[b])) ++ ")"
    show TBot = "!"


data TPair = TPair [TAtom] [TSubtype]

instance Show TPair where
    show (TPair a b) = show a ++ " " ++ show b


data TAtom = TCtor String
           | TFree String

instance Show TAtom where
    showList [] = showString "?"
    showList [x] = showString $ show x
    showList xs = showString $ "[" ++ (concat $ intersperse "," $ map show xs) ++ "]"
    
    show (TCtor x) = if x == "[]" then "#" else x
    show (TFree x) = x


typeySolve2 :: String -> Handle -> Hite -> DataM SmallT -> Func2M -> IO Bool
typeySolve2 file hndl hite datam funcm =
    do
        outBoth "-- TYPES OF FUNCTIONS"
        out $ unlines [a ++ " :: " ++ show b | (a,b) <- funcm]
        outBoth "-- SUBTYPES"
        out $ showLines $ addBottoms $ getSubtypes datam (fromJust $ lookup "head" funcm)
        return False
    where
        out = hPutStrLn hndl
        outBoth x = putStrLn x >> out x






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
renameVars = 1





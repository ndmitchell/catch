
module Typey.Solve2(typeySolve2) where

import Typey.Type
import IO
import Hite
import General.General
import Data.Maybe
import Data.List


data TSubtype = TAtom [TAtom]
              | TBind [TPair]
              | TArr TSubtype TSubtype
              | TBot

instance Show TSubtype where
    show (TAtom x) = show x
    show (TBind xs) = "{" ++ (concat $ intersperse " | " $ map show xs) ++ "}"
    show (TArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show TBot = "_|_"


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
        out $ showLines $ getSubtypes datam (fromJust $ lookup "head" funcm)
        return False
    where
        out = hPutStrLn hndl
        outBoth x = putStrLn x >> out x






getSubtypes :: DataM SmallT -> Large2T -> [TSubtype]
getSubtypes datam (Free2T a) = [TAtom [TFree a]]
getSubtypes datam (Arr2T a b) = [TArr x y | x <- getSubtypes datam a, y <- getSubtypes datam b]
getSubtypes datam (Ctor2T a) = getSubtypes datam $ Bind2T (Ctor2T a) []

getSubtypes datam (Bind2T (Ctor2T x) xs) = 
        [TBind [TPair [a] b] | a <- nas, b <- bs] ++
        [TBind [TPair [a1] b1, TPair [a2] b2] | a1 <- ras, b1 <- bs, a2 <- ras ++ nas, b2 <- bs]
    where
        (ras,nas) = (\(a,b) -> (map snd a,map snd b)) $ partition fst
            [(isRecursive ctr, TCtor q) | let DataT _ y = fromJust $ lookup x datam, ctr@(CtorT q _) <- y]
        bs = crossProduct $ map (getSubtypes datam) xs



-- add bottom everywhere you can
addBottoms = 1

-- rename all variables
renameVars = 1





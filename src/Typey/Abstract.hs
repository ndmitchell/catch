
module Typey.Abstract where

import Typey.Type
import General.General
import Hite
import Data.List
import Data.Maybe
import Control.Exception
import Debug.Trace


data Abstract = Bit Bool
              | List [Abstract]
              | AbsBottom
              | AbsVoid
              deriving Eq

b0 = Bit False
b1 = Bit True

fromBit (Bit x) = x

instance Show Abstract where
    show (Bit x) = if x then "1" else "0"
    show (AbsBottom) = "!"
    show (AbsVoid) = "#"
    show (List xs) = "[" ++ concatMap show xs ++ "]"



unionAbs :: [Abstract] -> Abstract
unionAbs xs = foldr f AbsVoid xs
    where
        f AbsVoid x = x
        f x AbsVoid = x
        f AbsBottom (List (x:xs)) = List (b1:xs)
        f (List (x:xs)) AbsBottom = List (b1:xs)
        f (Bit i) (Bit j) = Bit (i || j)
        f (List i) (List j) = List $ zipWithEq f i j




-- List [b0,b0,b1,List[b0],b1,b1,List[b0]]

getAbstract :: DataM SmallT -> Large2T -> Abstract
getAbstract datam x = f x
    where
        f (Free2T _) = List [b0]
        f (Arr2T _ _) = error "getAbstract: Cannot abstract for a higher order function"
        f (Bind2T (Ctor2T ctor) args) = List $ b0 : lst ++ rlst
            where
                rlst = if any isRecursive ctrs then lst else []
                lst = replicate (length ctrs) b1 ++ map f args
                (DataT n ctrs) = lookupJust ctor datam


hasCtorAbs :: DataM SmallT -> Abstract -> CtorName -> Bool
hasCtorAbs datam (List x) name = fromBit (x !! i)
    where
        i = head [i | (_, DataT n ctrs) <- datam,
                      (i, CtorT n _) <- zip [1..] ctrs, n == name]


followSelAbs :: Hite -> DataM SmallT -> Abstract -> String -> Abstract
followSelAbs hite datam (List (x:xs)) name = assert (length (x:xs) == len) $
        case as !! i of
            Self -> List $ x : take lenHalf xs ++ take lenHalf xs
            FreeS i -> xs !! (i + length ctrs)
    where
        lenHalf = length ctrs + n
        len = 1 + (lenHalf * (if rec then 2 else 1))
        rec = any isRecursive ctrs
        (DataT n ctrs, CtorT _ as) = head [(d,c) | (_,d@(DataT n cs)) <- datam,
                                                   c@(CtorT n2 _) <- cs,
                                                   n2 == cname]
        
        i = fromJust $ elemIndex name args
        Ctor cname args = getCtorFromArg hite name


makeAbs :: DataM SmallT -> CtorName -> [Abstract] -> Abstract
makeAbs datam name args = assert (length as == length args && length void == len) $
        res
    where
        res = List (foldr f (addBase void) (zip as args))
        addBase x = x !!! (i+1, b1)
        
        f :: (SmallT, Abstract) -> [Abstract] -> [Abstract]
        f (FreeS i, b) x = x !!! (ii, unionAbs [b, x !! ii])
            where ii = i + length ctrs + 1
    
        f (Self, AbsVoid) x = x
        f (Self, List (b:bs)) (x:xs) = unionAbs [x,b] : xs1 ++ bss
            where
                (xs1,xs2) = splitAt lenHalf xs
                (bs1,bs2) = splitAt lenHalf bs
                List bss = unionAbs [List bs1, List bs2, List xs2]
    
        f a b = error $ "makeAbs.f " ++ show (a,b)
    
        void = b0 : concat (replicate (if rec then 2 else 1) voidHalf)
        voidHalf = replicate (length ctrs) b0 ++ replicate n AbsVoid
        
        lenHalf = length ctrs + n
        len = 1 + (lenHalf * (if rec then 2 else 1))
        rec = any isRecursive ctrs
        (DataT n ctrs, CtorT _ as, i) = head [(d,c,i) | (_,d@(DataT n cs)) <- datam,
                                                   (i, c@(CtorT n2 _)) <- zip [0..] cs,
                                                   n2 == name]

unionAbsTrace msg x =
        trace (show msg ++ ": unionAbs " ++ show x) $
        trace (show res) res
    where res = unionAbs x


module Typey.Abstract where

import Typey.Type
import General.General
import Hite
import Data.List
import Data.Maybe
import Control.Exception
import Debug.Trace


data Abstract = Bit Bool
              | List Bool [Abstract] [Abstract]
              | AbsBottom
              | AbsVoid
              deriving Eq


fromBit (Bit x) = x

instance Show Abstract where
    show (Bit x) = if x then "1" else "0"
    show (AbsBottom) = "!"
    show (AbsVoid) = "#"
    show (List b xs ys) = "[" ++ bot ++ concatMap show (xs++ys) ++ "]"
        where bot = if b then "!" else "_"


absBottom :: Abstract -> Bool
absBottom (AbsBottom) = True
absBottom (List b xs ys) = b || any absBottom (xs++ys)
absBottom _ = False


unionAbs :: [Abstract] -> Abstract
unionAbs = unionAbsNote ""

unionAbsNote :: String -> [Abstract] -> Abstract
unionAbsNote msg xs = foldr f AbsVoid xs
    where
        f AbsVoid x = x
        f x AbsVoid = x
        f AbsBottom (List b xs ys) = List True xs ys
        f (List b xs ys) AbsBottom = List True xs ys
        f (Bit i) (Bit j) = Bit (i || j)
        f (List b1 xs1 ys1) (List b2 xs2 ys2) = List (b1||b2) (zipWithEq f xs1 xs2) (zipWithEq f ys1 ys2)
        f a b = error $ "unionAbs (" ++ msg ++ ") failed on " ++ show xs ++ " with " ++ show (a,b)




-- List [b0,b0,b1,List[b0],b1,b1,List[b0]]

getAbstract :: DataM SmallT -> Large2T -> Abstract
getAbstract datam x = f x
    where
        f (Free2T _) = List False [] []
        f (Arr2T _ _) = error "getAbstract: Cannot abstract for a higher order function"
        f (Ctor2T ctor) = f $ Bind2T (Ctor2T ctor) []
        
        f (Bind2T (Ctor2T ctor) args) = List False lst rlst
            where
                rlst = if any isRecursive ctrs then lst else []
                lst = replicate (length ctrs) (Bit True) ++ map f args
                (DataT n ctrs) = lookupJust ctor datam


hasCtorAbs :: DataM SmallT -> Abstract -> CtorName -> Bool
hasCtorAbs datam (List b xs ys) name = fromBit (xs !! i)
    where
        i = head [i | (_, DataT n ctrs) <- datam,
                      (i, CtorT n _) <- zip [0..] ctrs, n == name]


followSelAbs :: Hite -> DataM SmallT -> Abstract -> String -> Abstract
followSelAbs hite datam (List b xs ys) name = assertNote "followSelAbs"
        (len - 1 == length (xs++ys) && length xs == lenHalf) $
        case as !! i of
            Self -> List b ys ys
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
makeAbs datam name args = assert (length as == length args) $
        foldr f (addBase void) (zip as args)
    where
        addBase (List b xs ys) = List b (xs !!! (i, Bit True)) ys
        
        f :: (SmallT, Abstract) -> Abstract -> Abstract
        f (FreeS i, b) (List x x1 x2) = List x (x1 !!! (ii, unionAbs [b, x1 !! ii])) x2
            where ii = i + length ctrs
        
        f (Self, AbsVoid) x = x
        f (Self, List b bs1 bs2) (List bx xs1 xs2) = List (b||bx) xs1 bss
            where bss = map unionAbs $ transpose [bs1, bs2, xs2]

        f a b = error $ "makeAbs.f " ++ show (a,b)
    
        void = List False voidHalf (if rec then voidHalf else [])
        voidHalf = replicate (length ctrs) (Bit False) ++ replicate n AbsVoid
        
        lenHalf = length ctrs + n
        len = 1 + (lenHalf * (if rec then 2 else 1))
        
        rec = any isRecursive ctrs
        (DataT n ctrs, CtorT _ as, i) = head [(d,c,i) | (_,d@(DataT n cs)) <- datam,
                                                   (i, c@(CtorT n2 _)) <- zip [0..] cs,
                                                   n2 == name]

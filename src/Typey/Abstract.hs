
module Typey.Abstract where

import Typey.Type
import General.General
import Hite
import Data.List
import Data.Maybe
import Control.Exception
import Debug.Trace


data Abstract a = Bit Bool
                | List Bool [Abstract a] [Abstract a]
                | AbsBottom
                | AbsVoid
                | AbsOther [a]
                deriving Eq


fromBit (Bit x) = x
isBit (Bit x) = True; isBit _ = False

instance Show a => Show (Abstract a) where
    show (Bit x) = if x then "1" else "0"
    show (AbsBottom) = "!"
    show (AbsVoid) = "#"
    show (AbsOther x) = "(OTHER " ++ show x ++ ")"
    show (List b xs ys) = "[" ++ bot ++ concatMap show (xs++ys) ++ "]"
        where bot = if b then "!" else "_"



-- basically assert that there are no other statements
liftAbs :: Abstract a -> Abstract b
liftAbs (Bit x) = Bit x
liftAbs (List b xs ys) = List b (map liftAbs xs) (map liftAbs ys)
liftAbs AbsBottom = AbsBottom
liftAbs AbsVoid = AbsVoid


absBottom :: Abstract a -> Bool
absBottom (AbsBottom) = True
absBottom (List b xs ys) = b || any absBottom (xs++ys)
absBottom _ = False


headBottom :: Abstract a -> Bool
headBottom (AbsBottom) = True
headBottom (List b xs ys) = b
headBottom _ = False


unionAbs :: Show a => [Abstract a] -> Abstract a
unionAbs = unionAbsNote ""

unionAbsNote :: Show a => String -> [Abstract a] -> Abstract a
unionAbsNote msg xs = foldr f AbsVoid xs
    where
        f AbsVoid x = x
        f x AbsVoid = x
        f AbsBottom (List b xs ys) = List True xs ys
        f (List b xs ys) AbsBottom = List True xs ys
        f AbsBottom AbsBottom = AbsBottom
        f (Bit i) (Bit j) = Bit (i || j)
        f (AbsOther x) (AbsOther y) = AbsOther (x ++ y)
        f (List b1 xs1 ys1) (List b2 xs2 ys2) = List (b1||b2) (zipWithEq f xs1 xs2) (zipWithEq f ys1 ys2)
        f a b = error $ "unionAbs (" ++ msg ++ ") failed on " ++ show xs ++ " with " ++ show (a,b)


permuteAbs :: Abstract a -> [Abstract a]
permuteAbs x@(List b xs ys)
        | lts <= 1 = [x]
        | otherwise = [List b (bs++others) ys | i <- [0..lbs-1], bools !! i,
                                                let bs = replicate lbs (Bit False) !!! (i, Bit True)]
    where
        lts = length $ filter (== True) bools
        lbs = length bools
        bools = map fromBit bits
        (bits,others) = span isBit xs

permuteAbs x = [x]


-- List [b0,b0,b1,List[b0],b1,b1,List[b0]]

getAbstract :: DataM SmallT -> Large2T -> Abstract ()
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


hasCtorAbs :: DataM SmallT -> Abstract a -> CtorName -> Bool
hasCtorAbs datam AbsVoid _ = False
hasCtorAbs datam (List b xs ys) name = fromBit (xs !! i)
    where
        i = head [i | (_, DataT n ctrs) <- datam,
                      (i, CtorT n _) <- zip [0..] ctrs, n == name]



followSelAbs :: Hite -> DataM SmallT -> Abstract a -> String -> Abstract a
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


makeAbs :: Show a => DataM SmallT -> CtorName -> [Abstract a] -> Abstract a
makeAbs datam name args = assert (length as == length args) $
        foldr f (addBase void) (zip as args)
    where
        addBase (List b xs ys) = List b (xs !!! (i, Bit True)) ys
        
        f :: Show a => (SmallT, Abstract a) -> Abstract a -> Abstract a
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

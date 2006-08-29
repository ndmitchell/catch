
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
                | AbsAny -- any value except bottom
                | AbsTop -- any valud including bottom
                | AbsOther [a]
                | AbsFunc (AbsFunc a)
                deriving Eq

data AbsFunc a = AbsFuncName FuncName
               | AbsFuncUnion [AbsFunc a]
               | AbsApply (AbsFunc a) [Abstract a]
               deriving Eq


fromBit (Bit x) = x
isBit (Bit x) = True; isBit _ = False

instance Show a => Show (Abstract a) where
    show (Bit x) = if x then "1" else "0"
    show (AbsBottom) = "!"
    show (AbsVoid) = "#"
    show (AbsAny) = "+"
    show (AbsFunc x) = show x
    show (AbsTop) = "*" -- union Bottom and Any
    show (AbsOther x) = "(OTHER " ++ show x ++ ")"
    show (List b xs ys) = "[" ++ bot ++ concatMap show (xs++ys) ++ "]"
        where bot = if b then "!" else "_"

instance Show a => Show (AbsFunc a) where
    show (AbsFuncName x) = x
    show (AbsFuncUnion xs) = show xs
    show (AbsApply f xs) = "(" ++ show f ++ concatMap ((' ':) . show) xs ++ ")"


-- basically assert that there are no other statements
liftAbs :: Abstract a -> Abstract b
liftAbs (Bit x) = Bit x
liftAbs (List b xs ys) = List b (map liftAbs xs) (map liftAbs ys)
liftAbs AbsBottom = AbsBottom
liftAbs AbsVoid = AbsVoid
liftAbs AbsAny = AbsAny
liftAbs AbsTop = AbsTop


absFunc :: (a -> [FuncName]) -> Abstract a -> [FuncName]
absFunc f (List _ a b) = concatMap (absFunc f) (a++b)
absFunc f (AbsOther xs) = concatMap f xs
absFunc f (AbsFunc x) = g x
    where
        g (AbsFuncName x) = [x]
        g (AbsFuncUnion x) = concatMap g x
        g (AbsApply x xs) = g x ++ concatMap (absFunc f) xs
absFunc f x = []



absBottom :: Abstract a -> Bool
absBottom (AbsBottom) = True
absBottom (AbsTop) = True
absBottom (List b xs ys) = b || any absBottom (xs++ys)
absBottom _ = False


headBottom :: Abstract a -> Bool
headBottom (AbsBottom) = True
headBottom (AbsTop) = True
headBottom (List b xs ys) = b
headBottom _ = False


unionAbs :: (Eq a, Show a) => [Abstract a] -> Abstract a
unionAbs = unionAbsNote ""

unionAbsNote :: (Eq a, Show a) => String -> [Abstract a] -> Abstract a
unionAbsNote msg xs = foldr f AbsVoid xs
    where
        f AbsVoid x = x
        f x AbsVoid = x
        f AbsBottom (List b xs ys) = List True xs ys
        f (List b xs ys) AbsBottom = List True xs ys
        f AbsBottom AbsBottom = AbsBottom
        f (Bit i) (Bit j) = Bit (i || j)
        f (AbsOther x) (AbsOther y) = AbsOther (nub $ x ++ y)
        f (List b1 xs1 ys1) (List b2 xs2 ys2) = List (b1||b2) (zipWithEq f xs1 xs2) (zipWithEq f ys1 ys2)
        f (AbsAny) (AbsAny) = AbsAny
        f AbsAny x@(List{}) = makeAny x
        f x@(List{}) AbsAny = makeAny x
        f AbsTop _ = AbsTop
        f _ AbsTop = AbsTop
        f AbsAny AbsBottom = AbsTop
        f AbsBottom AbsAny = AbsTop
        f a b = error $ "unionAbs (" ++ msg ++ ") failed on " ++ show xs ++ " with " ++ show (a,b)


-- consider it as union with AbsAny, but keeping the type structure
makeAny :: Show a => Abstract a -> Abstract a
makeAny (Bit i) = Bit True
makeAny (List b xs ys) = List b (map makeAny xs) (map makeAny ys)
makeAny AbsVoid = AbsAny
makeAny x = error $ "Abstract.makeAny: " ++ show x


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


hasCtorAbs :: Show a => DataM SmallT -> Abstract a -> CtorName -> Bool
hasCtorAbs datam AbsAny _ = True
hasCtorAbs datam AbsVoid _ = False
hasCtorAbs datam (List b xs ys) name = fromBit (xs !! i)
    where
        i = head [i | (_, DataT n ctrs) <- datam,
                      (i, CtorT n _) <- zip [0..] ctrs, n == name]

hasCtorAbs datam x y = error $ show ("hasCtorAbs unhandled",x,y)


followSelAbs :: Hite -> DataM SmallT -> Abstract a -> String -> Abstract a
followSelAbs hite datam AbsAny name = AbsAny
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
        Ctor cname args _ = rawCtor $ getCArg hite name


makeAbs :: (Eq a, Show a) => DataM SmallT -> CtorName -> [Abstract a] -> Abstract a
makeAbs datam name args = assert (length as == length args) $
        foldr f (addBase void) (zip as args)
    where
        addBase (List b xs ys) = List b (xs !!! (i, Bit True)) ys
        
        f :: (Eq a, Show a) => (SmallT, Abstract a) -> Abstract a -> Abstract a
        f (FreeS i, b) (List x x1 x2) = List x (x1 !!! (ii, unionAbs [b, x1 !! ii])) x2
            where ii = i + length ctrs
        
        f (Self, AbsVoid) x = x
        
        f (Self, AbsAny) (List bx xs1 xs2) = List bx xs1 (map makeAny xs2)
        
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

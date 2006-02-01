
module Preamble where

import qualified Prelude
import Prelude( []((:),[]) , Bool(True,False) )


-- SPECIAL FUNCTIONS

-- yhc bottoms out if some of these are not imported

undefined = undefined

_apply1 f x = f x


preamble_error_empty_list = []
preamble_error msg = case preamble_error_empty_list of
    (_:_) -> undefined


prim_STRING x = x
prim_str = undefined


-- STANDARD FUNCTIONS
head (x:xs) = x

map f [] = []
map f (x:xs) = f x : map f xs


reverse xs = reverse_acc xs []
    where
        reverse_acc [] ys = ys
        reverse_acc (x:xs) ys = reverse_acc xs (x:ys)

foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

null [] = True
null _ = False


True || _ = True
False || a = a

flip f x y  = f y x



class Eq a  where
    (==), (/=)      :: a -> a -> Bool

    x /= y      =  not (x == y)
    x == y      =  not (x /= y)


not True = False
not False = True


class  (Eq a) => Ord a  where
    compare     :: a -> a -> Ordering
    (<), (<=), (>=), (>):: a -> a -> Bool
    max, min        :: a -> a -> a

    compare x y = case x == y of
                    True -> EQ
                    False -> case x <= y of
                            True -> LT
                            False -> GT

    x <= y      = compare x y /= GT
    x <  y      = compare x y == LT
    x >= y      = compare x y /= LT
    x >  y      = compare x y == GT

    max x y = case x >= y of
                True -> y
                False -> x

    min x y = case x >= y of
                True -> x
                False -> y



data Ordering = LT | EQ | GT


instance Eq Ordering where
    LT == LT = True
    EQ == EQ = True
    GT == GT = True
    _ == _ = False



data Int = Int


instance Eq Int where
    a == b = undefined


instance Ord Int where
    compare a b = undefined

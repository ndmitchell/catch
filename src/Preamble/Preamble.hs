
module Preamble where

{-
GENERAL NOTES:

Yhc requires certain bits of the prelude to be available,
so importing Prelude() means it won't compile. However qualified Prelude
seems to get most of the way there.

Some things however require a non-qualified import, such as [] and Bool.
-}


-- special imports
import qualified Prelude
import Prelude( []((:),[]) , Bool(True,False) )



---------------------------------------------------------------------
-- YHC.Internal
--
-- Special functions, definitions copied directly

_apply1 f x = f x
_apply2 f x y = f x y
_apply3 f x y z = f x y z
_apply4 f x y z u = f x y z u
_id x = x



---------------------------------------------------------------------
-- Special
--
-- Functions which are very Yhc/Catch specific

-- catch_any is desugared to _, which is all possible values
-- used to encode non-determinism
-- treated specially by the checker
catch_any = catch_any


prim_STRING x = x

prim_str = catch_any
prim_EQ_W a b = catch_any
prim_ORD a b = catch_any

{-

data Tup1 a1 = Tup1 {tup1_1 :: a1}
data Tup2 a1 a2 = Tup2 {tup2_1 :: a1, tup2_2 :: a2}
data Tup3 a1 a2 a3 = Tup3 {tup3_1 :: a1, tup3_2 :: a2, tup3_3 :: a3}
data Tup4 a1 a2 a3 a4 = Tup4 {tup4_1 :: a1, tup4_2 :: a2, tup4_3 :: a3, tup4_4 :: a4}
data Tup5 a1 a2 a3 a4 a5 = Tup5 {tup5_1 :: a1, tup5_2 :: a2, tup5_3 :: a3, tup5_4 :: a4, tup5_5 :: a5}
data Tup6 a1 a2 a3 a4 a5 a6 = Tup6 {tup6_1 :: a1, tup6_2 :: a2, tup6_3 :: a3, tup6_4 :: a4, tup6_5 :: a5, tup6_6 :: a6}
data Tup7 a1 a2 a3 a4 a5 a6 a7 = Tup7 {tup7_1 :: a1, tup7_2 :: a2, tup7_3 :: a3, tup7_4 :: a4, tup7_5 :: a5, tup7_6 :: a6, tup7_7 :: a7}
data Tup8 a1 a2 a3 a4 a5 a6 a7 a8 = Tup8 {tup8_1 :: a1, tup8_2 :: a2, tup8_3 :: a3, tup8_4 :: a4, tup8_5 :: a5, tup8_6 :: a6, tup8_7 :: a7, tup8_8 :: a8}
data Tup9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = Tup9 {tup9_1 :: a1, tup9_2 :: a2, tup9_3 :: a3, tup9_4 :: a4, tup9_5 :: a5, tup9_6 :: a6, tup9_7 :: a7, tup9_8 :: a8, tup9_9 :: a9}

-}

---------------------------------------------------------------------
-- Prelude
--
-- Special functions, which have a different semantics in CATCH

error msg = case [] of
    (_:_) -> catch_any


---------------------------------------------------------------------
-- Prelude.General
--
-- Just general small functions

flip f x y  = f y x
undefined = error ""



---------------------------------------------------------------------
-- Prelude.List
--
-- the List constructor is considered to be internal, and is special cased
-- it really doesn't seem possible to write a custom one for MANY reasons

--   [] a = [] | : a [a]
data Preamble_Hex_5B5D a = Preamble_Hex_5B5D | Preamble_Hex_3A {hd :: a, tl :: [a]}


head (x:xs) = x

map f [] = []
map f (x:xs) = f x : map f xs

-- Hugs implementation that I don't understand :)
-- reverse = foldl (flip (:)) []
reverse xs = reverse_acc xs []
    where
        reverse_acc [] ys = ys
        reverse_acc (x:xs) ys = reverse_acc xs (x:ys)

foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

null [] = True
null _ = False

---------------------------------------------------------------------
-- Prelude.Bool
--
-- everything to do with booleans

data Preamble_Bool = Preamble_False | Preamble_True

True  || _ = True
False || a = a

True  && a = a
False && _ = False

not True = False
not False = True


{-

---------------------------------------------------------------------
-- Prelude.Comparison
--
-- Eq and Ord declarations
-- if its Eq directly, then I can't use deriving in this module

data Ordering = LT | EQ | GT
                deriving Prelude.Eq

class Preamble_Eq a  where
    (==), (/=)      :: a -> a -> Bool

    x /= y      =  not (x == y)
    x == y      =  not (x /= y)


class  (Preamble_Eq a) => Preamble_Ord a  where
    compare :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min :: a -> a -> a

    compare x y
      | x == y    = EQ
      | x <= y    = LT
      | True      = GT

    x <= y      = (Prelude./=) (compare x y) GT
    x <  y      = (Prelude.==) (compare x y) LT
    x >= y      = (Prelude./=) (compare x y) LT
    x >  y      = (Prelude.==) (compare x y) GT

    max x y | x >= y = x
            | True   = y
    min x y | x <= y = x
            | True   = y



---------------------------------------------------------------------
-- Prelude.Numeric
--
-- Int

data Int = Int

instance Prelude.Eq Int where
    a == b = catch_any

instance Prelude.Ord Int where
    compare a b = catch_any
-}

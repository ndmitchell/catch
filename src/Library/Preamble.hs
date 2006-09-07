
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
import Prelude( []((:),[]) , Bool(True,False), Char)



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
ignore x = catch_any
prim = catch_any


prim_STRING x = x

prim_int = catch_any
prim_EQ_W a b = prim
prim_GT_W a b = prim
prim_ORD a = prim
prim_ADD_W a b = prim
prim_LE_W a b = prim
prim_LT_W a b = prim


data Tup0 = Tup0
data Tup1 a1 = Tup1 {tup1_1 :: a1}
data Tup2 a1 a2 = Tup2 {tup2_1 :: a1, tup2_2 :: a2}
data Tup3 a1 a2 a3 = Tup3 {tup3_1 :: a1, tup3_2 :: a2, tup3_3 :: a3}
data Tup4 a1 a2 a3 a4 = Tup4 {tup4_1 :: a1, tup4_2 :: a2, tup4_3 :: a3, tup4_4 :: a4}
data Tup5 a1 a2 a3 a4 a5 = Tup5 {tup5_1 :: a1, tup5_2 :: a2, tup5_3 :: a3, tup5_4 :: a4, tup5_5 :: a5}
data Tup6 a1 a2 a3 a4 a5 a6 = Tup6 {tup6_1 :: a1, tup6_2 :: a2, tup6_3 :: a3, tup6_4 :: a4, tup6_5 :: a5, tup6_6 :: a6}
data Tup7 a1 a2 a3 a4 a5 a6 a7 = Tup7 {tup7_1 :: a1, tup7_2 :: a2, tup7_3 :: a3, tup7_4 :: a4, tup7_5 :: a5, tup7_6 :: a6, tup7_7 :: a7}
data Tup8 a1 a2 a3 a4 a5 a6 a7 a8 = Tup8 {tup8_1 :: a1, tup8_2 :: a2, tup8_3 :: a3, tup8_4 :: a4, tup8_5 :: a5, tup8_6 :: a6, tup8_7 :: a7, tup8_8 :: a8}
data Tup9 a1 a2 a3 a4 a5 a6 a7 a8 a9 = Tup9 {tup9_1 :: a1, tup9_2 :: a2, tup9_3 :: a3, tup9_4 :: a4, tup9_5 :: a5, tup9_6 :: a6, tup9_7 :: a7, tup9_8 :: a8, tup9_9 :: a9}
data Tup10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = Tup10 {tup10_1 :: a1, tup10_2 :: a2, tup10_3 :: a3, tup10_4 :: a4, tup10_5 :: a5, tup10_6 :: a6, tup10_7 :: a7, tup10_8 :: a8, tup10_9 :: a9, tup10_10 :: a10}
data Tup11 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = Tup11 {tup11_1 :: a1, tup11_2 :: a2, tup11_3 :: a3, tup11_4 :: a4, tup11_5 :: a5, tup11_6 :: a6, tup11_7 :: a7, tup11_8 :: a8, tup11_9 :: a9, tup11_10 :: a10, tup11_11 :: a11}
data Tup12 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 = Tup12 {tup12_1 :: a1, tup12_2 :: a2, tup12_3 :: a3, tup12_4 :: a4, tup12_5 :: a5, tup12_6 :: a6, tup12_7 :: a7, tup12_8 :: a8, tup12_9 :: a9, tup12_10 :: a10, tup12_11 :: a11, tup12_12 :: a12}
data Tup13 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 = Tup13 {tup13_1 :: a1, tup13_2 :: a2, tup13_3 :: a3, tup13_4 :: a4, tup13_5 :: a5, tup13_6 :: a6, tup13_7 :: a7, tup13_8 :: a8, tup13_9 :: a9, tup13_10 :: a10, tup13_11 :: a11, tup13_12 :: a12, tup13_13 :: a13}
data Tup14 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 = Tup14 {tup14_1 :: a1, tup14_2 :: a2, tup14_3 :: a3, tup14_4 :: a4, tup14_5 :: a5, tup14_6 :: a6, tup14_7 :: a7, tup14_8 :: a8, tup14_9 :: a9, tup14_10 :: a10, tup14_11 :: a11, tup14_12 :: a12, tup14_13 :: a13, tup14_14 :: a14}
data Tup15 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 = Tup15 {tup15_1 :: a1, tup15_2 :: a2, tup15_3 :: a3, tup15_4 :: a4, tup15_5 :: a5, tup15_6 :: a6, tup15_7 :: a7, tup15_8 :: a8, tup15_9 :: a9, tup15_10 :: a10, tup15_11 :: a11, tup15_12 :: a12, tup15_13 :: a13, tup15_14 :: a14, tup15_15 :: a15}
data Tup16 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 = Tup16 {tup16_1 :: a1, tup16_2 :: a2, tup16_3 :: a3, tup16_4 :: a4, tup16_5 :: a5, tup16_6 :: a6, tup16_7 :: a7, tup16_8 :: a8, tup16_9 :: a9, tup16_10 :: a10, tup16_11 :: a11, tup16_12 :: a12, tup16_13 :: a13, tup16_14 :: a14, tup16_15 :: a15, tup16_16 :: a16}
data Tup17 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 = Tup17 {tup17_1 :: a1, tup17_2 :: a2, tup17_3 :: a3, tup17_4 :: a4, tup17_5 :: a5, tup17_6 :: a6, tup17_7 :: a7, tup17_8 :: a8, tup17_9 :: a9, tup17_10 :: a10, tup17_11 :: a11, tup17_12 :: a12, tup17_13 :: a13, tup17_14 :: a14, tup17_15 :: a15, tup17_16 :: a16, tup17_17 :: a17}
data Tup18 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 = Tup18 {tup18_1 :: a1, tup18_2 :: a2, tup18_3 :: a3, tup18_4 :: a4, tup18_5 :: a5, tup18_6 :: a6, tup18_7 :: a7, tup18_8 :: a8, tup18_9 :: a9, tup18_10 :: a10, tup18_11 :: a11, tup18_12 :: a12, tup18_13 :: a13, tup18_14 :: a14, tup18_15 :: a15, tup18_16 :: a16, tup18_17 :: a17, tup18_18 :: a18}
data Tup19 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 = Tup19 {tup19_1 :: a1, tup19_2 :: a2, tup19_3 :: a3, tup19_4 :: a4, tup19_5 :: a5, tup19_6 :: a6, tup19_7 :: a7, tup19_8 :: a8, tup19_9 :: a9, tup19_10 :: a10, tup19_11 :: a11, tup19_12 :: a12, tup19_13 :: a13, tup19_14 :: a14, tup19_15 :: a15, tup19_16 :: a16, tup19_17 :: a17, tup19_18 :: a18, tup19_19 :: a19}
data Tup20 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 = Tup20 {tup20_1 :: a1, tup20_2 :: a2, tup20_3 :: a3, tup20_4 :: a4, tup20_5 :: a5, tup20_6 :: a6, tup20_7 :: a7, tup20_8 :: a8, tup20_9 :: a9, tup20_10 :: a10, tup20_11 :: a11, tup20_12 :: a12, tup20_13 :: a13, tup20_14 :: a14, tup20_15 :: a15, tup20_16 :: a16, tup20_17 :: a17, tup20_18 :: a18, tup20_19 :: a19, tup20_20 :: a20}

instance Preamble_Eq Tup2 where
    a == b = catch_any

instance Preamble_Ord Tup2 where
    compare a b = catch_any

---------------------------------------------------------------------
-- Prelude
--
-- Special functions, which have a different semantics in CATCH

error msg = error msg

---------------------------------------------------------------------
-- Prelude.General
--
-- Just general small functions

flip f x y  = f y x
undefined = error ""
otherwise = True
(.) f g x = f (g x)
($) f x = f x
fst (a,b) = a
snd (a,b) = b
id x = x

---------------------------------------------------------------------
-- Prelude.List
--
-- the List constructor is considered to be internal, and is special cased
-- it really doesn't seem possible to write a custom one for MANY reasons

--   [] a = [] | : a [a]
data Preamble_Hex_5B5D a = Preamble_Hex_5B5D | Preamble_Hex_3A {hd :: a, tl :: [a]}
                           deriving (Prelude.Eq, Prelude.Ord)


head (x:xs) = x

tail (x:xs) = xs

map f [] = []
map f (x:xs) = f x : map f xs

filter f [] = []
filter f (x:xs) = if f x then x:res else res
    where res = filter f xs

_filter b e r = if b then e r else r

concat = foldr (++) []

concatMap f xs = concat (map f xs)

-- Hugs implementation that I don't understand :)
-- reverse = foldl (flip (:)) []
reverse xs = reverse_acc xs []
    where
        reverse_acc [] ys = ys
        reverse_acc (x:xs) ys = reverse_acc xs (x:ys)

foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

-- NOTE! Different argument orders, this is just STUPID!!!
_foldr f [] d = d
_foldr f ((:) x xs) d = f x (_foldr f xs d)

foldr1 f [x]      = x
foldr1 f (x:xs)   = f x (foldr1 f xs)

null [] = True
null _ = False

length _ = catch_any

[]     ++ ys      = ys
(x:xs) ++ ys      = x : (xs ++ ys)

take :: Int -> [a] -> [a]
take n _  | n <= Int_0  = []
take _ []           = []
take n (x:xs)       = x : take (n-Int_1) xs


takeWhile           :: (a -> Bool) -> [a] -> [a]
takeWhile p []       = []
takeWhile p (x:xs)
     | p x       = x : takeWhile p xs
     | otherwise = []

dropWhile           :: (a -> Bool) -> [a] -> [a]
dropWhile p []       = []
dropWhile p xs@(x:xs')
     | p x       = dropWhile p xs'
     | otherwise = xs

span, break         :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p xs@(x:xs')
     | p x       = (x:ys, zs)
     | otherwise = ([],xs)
                       where (ys,zs) = span p xs'
break p              = span (not . p)

and        x = foldr (&&) True x
or         x = foldr (||) False x

any p x = or  (map p x)
all p x = and (map p x)

elem              = any . (==)
notElem           = all . (/=)

repeat x          = x : repeat x

last [x] = x
last (x:xs) = last xs

---------------------------------------------------------------------
-- Prelude.Bool
--
-- everything to do with booleans

data Preamble_Bool = Preamble_False | Preamble_True
    deriving (Prelude.Show)

True  || _ = True
False || a = a

True  && a = a
False && _ = False

not True = False
not False = True


---------------------------------------------------------------------
-- Prelude.Char
--
-- everything to do with characters

data Preamble_Char = Char


instance Prelude.Eq Preamble_Char where
    _ == _ = catch_any

instance Prelude.Ord Preamble_Char where
    compare _ _ = catch_any


---------------------------------------------------------------------
-- Prelude.Show
--

type String = [Char]

showChar      = (:)

showString    = (++)

shows         = showsPrec Int_0

type ShowS   = String -> String

class Preamble_Show a where
    show      :: a -> String
    showsPrec :: Int -> a -> ShowS
    showList  :: [a] -> ShowS
    showsType :: a -> ShowS -- Yhc extra method!

    -- Minimal complete definition: show or showsPrec
    show x          = showsPrec Int_0 x (ignore "")
    showsPrec _ x s = show x ++ s
    showList []     = showString (ignore "[]")
    showList (x:xs) = showChar (ignore '[') . shows x . showl xs
        where
            showl []     = showChar (ignore ']')
            showl (x:xs) = showChar (ignore ',') . shows x . showl xs
    showsType x = showString ""

instance Preamble_Show Int where
    show x = catch_any

instance Preamble_Show Integer where
    show x = catch_any


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

class (Preamble_Eq a, Preamble_Show a) => Preamble_Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs, signum    :: a -> a
    fromInteger    :: Integer -> a
    fromInt        :: Int -> a

    -- Minimal complete definition: All, except negate or (-)
    x - y           = x + negate y
    fromInt         = fromIntegral
    negate x        = fromInt Int_0 - x


toInteger x = catch_any

fromIntegral = fromInteger . toInteger

data Int = Int | Int_0 | Int_1

data Integer = Integer | Integer_0 | Integer_1

instance Preamble_Eq Int where
    a == b = catch_any

instance Preamble_Ord Int where
    compare a b = catch_any

instance Preamble_Num Int where
    (+) a b = catch_any
    (-) a b = catch_any
    (*) a b = catch_any
    abs a = catch_any
    fromInteger x = catch_any
    signum a = catch_any

instance Preamble_Eq Integer where
    a == b = catch_any

instance Preamble_Ord Integer where
    compare a b = catch_any

instance Preamble_Num Integer where
    (+) a b = catch_any
    (-) a b = catch_any
    (*) a b = catch_any
    abs a = catch_any
    fromInteger x = catch_any
    signum a = catch_any


subtract        = flip (-)



---------------------------------------------------------------------
-- Prelude.Enum
--

class Preamble_Enum a where
    succ, pred           :: a -> a
    toEnum               :: Int -> a
    fromEnum             :: a -> Int
    enumFrom             :: a -> [a]              -- [n..]
    enumFromThen         :: a -> a -> [a]         -- [n,m..]
    enumFromTo           :: a -> a -> [a]         -- [n..m]
    enumFromThenTo       :: a -> a -> a -> [a]    -- [n,n'..m]

    -- Minimal complete definition: toEnum, fromEnum
    succ                  = toEnum . (+Int_1)       . fromEnum
    pred                  = toEnum . subtract Int_1 . fromEnum
    enumFrom x            = catch_any -- map toEnum [ fromEnum x ..]
    enumFromTo x y        = catch_any -- map toEnum [ fromEnum x .. fromEnum y ]
    enumFromThen x y      = catch_any -- map toEnum [ fromEnum x, fromEnum y ..]
    enumFromThenTo x y z  = catch_any -- map toEnum [ fromEnum x, fromEnum y .. fromEnum z ]


instance Preamble_Enum Int where
    toEnum x = x
    fromEnum x = x


_fromEnum x = catch_any


---------------------------------------------------------------------
-- Prelude.Monad
--

-- its very important to match the same order as Yhc for
-- these definitions
class Preamble_Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a

    -- Minimal complete definition: (>>=), return
    p >> q  = p >>= \ _ -> q
    fail s  = error s


data IO a = OI {io :: a}


instance Preamble_Monad IO where
    return x = OI x
    OI a >>= f = f a
    OI a >> b = b


getContents = prim

putChar x = prim

putStr []     = OI ()
putStr (x:xs) = putChar x >> putStr xs

putStrLn x = putStr (x ++ "\n")


print x = putStrLn (show x)

-- these functions can be defined as higher order, but are not
-- pragmatic reasons :)

-- if type signatures are NOT given then the type is wrong
-- it generates two dictionaries!

sequence :: Preamble_Monad a => [a b] -> a [b]
sequence []     = return []
sequence (c:cs) = c >>= \x ->
                  sequence cs >>= \xs ->
                  return (x:xs)

sequence_ :: Preamble_Monad a => [a b] -> a ()
sequence_ []     = return ()
sequence_ (c:cs) = c >> sequence_ cs

mapM :: Preamble_Monad a => (b -> a c) -> [b] -> a [c]
mapM f []     = return []
mapM f (c:cs) = f c >>= \x ->
                mapM f cs >>= \xs -> 
                return (x:xs)

mapM_ :: Preamble_Monad a => (b -> a c) -> [b] -> a ()
mapM_ f []     = return ()
mapM_ f (c:cs) = f c >> mapM_ f cs

f =<< x           = x >>= f

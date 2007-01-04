
-- override things in the Prelude

module Primitive where

import System.IO(Handle)
import Data.Char(ord)


foreign import primitive prim_local_1 :: a -> b
foreign import primitive prim_local_2 :: a -> b -> c


-- erroneous mkIO stuff, to catch bugs elsewhere
global_YHC'_Internal'__mkIOok2 :: (c->b->a) -> (c->b->IO a)
global_YHC'_Internal'__mkIOok2 = undef

global_YHC'_Internal'__mkIOok3 :: (d->c->b->a) -> (d->c->b->IO a)
global_YHC'_Internal'__mkIOok3 = undef

undef = error "undefined"


-- make error primitive
foreign import primitive global_Prelude'_error :: a -> b

-- things which are mutually recursive and useful
global_Prelude'_repeat x = x : global_Prelude'_repeat x
global_Prelude'_cycle x = x ++ global_Prelude'_cycle x

-- things which suck, mutually recursively
foreign import primitive global_Numeric'_fromRat'' :: a -> b -> c
foreign import primitive global_PreludeAux'__floatFromRational :: a -> b
foreign import primitive global_PreludeAux'__doubleFromRational :: a -> b

-- ones which have a too concrete implementation
foreign import primitive global_System'_IO'_hGetChar :: Handle -> IO Prelude.Char
foreign import primitive global_System'_IO'_stdin :: a
foreign import primitive global_System'_IO'_stdout :: a
foreign import primitive global_System'_IO'_stderr :: a
foreign import primitive global_System'_IO'_throwIOError :: a -> b
foreign import primitive global_System'_IO'_hPutChar :: a -> b -> c
foreign import primitive global_System'_IO'_openFile :: a -> b -> c
--foreign import primitive global_Prelude'_Prelude'_Integral'_Prelude'_Int'_divMod :: a -> b -> c
--foreign import primitive global_Prelude'_Prelude'_Integral'_Prelude'_Integer'_divMod :: a -> b -> c


-- IO stuff
data IO a = IO a

global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_return a = IO a
global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_'gt'gt (IO a) b = b
global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_'gt'gt'eq (IO a) f = f a

-- special things

-- since reverse is so often used, make it simple
global_Prelude'_reverse x = f x []
    where
        f [] acc = acc
        f (x:xs) acc = f xs (x:acc)


global_YHC'_Internal'_unsafePerformIO (IO a) = a

global_System'_IO'_hGetContents :: Handle -> IO String
global_System'_IO'_hGetContents hndl =
    case global_System'_IO'_hGetChar hndl of
        IO c -> if c == '\0'
                then IO []
                else IO (c : res)
    where
        res = global_YHC'_Internal'_unsafePerformIO (global_System'_IO'_hGetContents hndl)


-- throws up a bug in the let stuff
global_Numeric'_floatToDigits :: (RealFloat a) => Integer -> a -> ([Int], Int)
global_Numeric'_floatToDigits a b = prim_local_2 a b


-- generates too much data
global_Data'_Char'_showLitChar :: Char -> ShowS
global_Data'_Char'_showLitChar x y = prim_local_1 x : prim_local_1 x ++ y


-- implemented really badly!
global_Prelude'_Prelude'_Read'_Prelude'_Int'_readsPrec :: Int -> String -> [(Int,String)]
global_Prelude'_Prelude'_Read'_Prelude'_Int'_readsPrec a b = read_int b


global_Prelude'_Prelude'__'_readList :: (Int -> String -> [(a,String)], String -> [([a], String)]) -> String -> [([a], String)]
global_Prelude'_Prelude'__'_readList (read_item, _) s = read_list read_item s

global_Prelude'_read :: (Int -> String -> [(a,String)], String -> [([a], String)]) -> String -> a
global_Prelude'_read (a, b) s = case a 0 s of
                                    [(x,ys)] | spaces ys -> x
                                    _ -> error "Failed to parse"
    where
        spaces [] = True
        spaces (x:xs) = if isSpace x then spaces xs else False

isDigit x = x >= '0' && x <= '9'
intToDigit x = ord x - ord '0'
isSpace x = x `elem` " \t\r\n"
isAlpha x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')


read_int :: String -> [(Int,String)]
read_int x = f x
    where
        f (x:xs) | isSpace x = f xs
        f ('(':xs) = end_bracket (f xs)
        f ('-':xs) = case g xs of
                        [(i,s)] -> [(negate i, s)]
                        x -> x
        f xs = g xs
        
        g (x:xs) | isDigit x = h (intToDigit x) xs
        g _ = []
        
        h n (x:xs) | isDigit x = h (n*10 + intToDigit x) xs
                   | isAlpha x = []
        h n s = [(n,s)]



read_list :: (Int -> String -> [(a,String)]) -> String -> [([a], String)]
read_list read_one x = f x
    where
        f (x:xs) | isSpace x = f xs
        f ('(':xs) = end_bracket (f xs)
        f ('[':xs) = g xs
        
        g xs = case read_one 0 xs of
                   [(a,xs)] -> h [a] xs
                   [] -> []

        h as (']':xs) = [(reverse as, xs)]
        h as (',':xs) = case read_one 0 xs of
                           [(a,xs)] -> h (a:as) xs
                           [] -> []
        h as (x:xs) | isSpace x = h as xs
        h as _ = []


end_bracket :: [(a, String)] -> [(a, String)]
end_bracket [(a, ')':xs)] = [(a,xs)]
end_bracket [(a, x:xs)] | isSpace x = end_bracket [(a, xs)]
end_bracket _ = []

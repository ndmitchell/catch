
-- override things in the Prelude

module Primitive where

import System.IO(Handle)
import Data.Char(ord)


-- local definitions

foreign import primitive any0 :: a
foreign import primitive anyEval :: a -> b

any2 a b = if any0 then a else b
any3 a b c = any2 a (any2 b c)


-- things which suck, mutually recursively
foreign import primitive global_Numeric'_fromRat'' :: a -> b -> c
foreign import primitive global_PreludeAux'__floatFromRational :: a -> b
foreign import primitive global_PreludeAux'__doubleFromRational :: a -> b
foreign import primitive global_Numeric'_floatToDigits :: a -> b -> c


---------------------------------------------------------------------
-- Prelude

-- make error primitive
foreign import primitive global_Prelude'_error :: a -> b

-- things which are mutually recursive and useful
global_Prelude'_repeat x = x : global_Prelude'_repeat x
global_Prelude'_cycle x = x ++ global_Prelude'_cycle x

-- since reverse is so often used, make it simple
global_Prelude'_reverse x = f x []
    where
        f [] acc = acc
        f (x:xs) acc = f xs (x:acc)

-- may return an error, or a demonic value
-- cannot use read in a safe program!
global_Prelude'_read a b = any2 (error "Prelude.read") (anyEval b)


---------------------------------------------------------------------
-- System.IO


-- ones which have a too concrete implementation
foreign import primitive global_System'_IO'_hGetChar :: Handle -> IO Prelude.Char
foreign import primitive global_System'_IO'_hPutChar :: a -> b -> c
foreign import primitive global_System'_IO'_stdin :: a
foreign import primitive global_System'_IO'_stdout :: a
foreign import primitive global_System'_IO'_stderr :: a
foreign import primitive global_System'_IO'_throwIOError :: a -> b
foreign import primitive global_System'_IO'_openFile :: a -> b -> c
foreign import primitive global_System'_IO'_hGetContents :: Handle -> IO String


-- IO stuff
data IO a = IO a

global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_return a = IO a
global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_'gt'gt (IO a) b = b
global_Prelude'_Prelude'_Monad'_YHC'_Internal'_IO'_'gt'gt'eq (IO a) f = f a
global_YHC'_Internal'_unsafePerformIO (IO a) = a


---------------------------------------------------------------------
-- Data.Char

-- generates too much data
-- abstractly it generates 1..n characters
global_Data'_Char'_showLitChar :: Char -> ShowS
global_Data'_Char'_showLitChar x y = anyEval x : anyEval x ++ y


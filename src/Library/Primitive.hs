
-- override things in the Prelude

module Primitive where

import System.IO(Handle)

-- real primitive operations
foreign import primitive prim_ORD :: a -> b

foreign import primitive prim_EQ_W :: a -> b -> c
foreign import primitive prim_GT_W :: a -> b -> c
foreign import primitive prim_ADD_W :: a -> b -> c
foreign import primitive prim_LE_W :: a -> b -> c
foreign import primitive prim_LT_W :: a -> b -> c
foreign import primitive prim_SUB_W :: a -> b -> c
foreign import primitive prim_GE_W :: a -> b -> c
foreign import primitive prim_NE_W :: a -> b -> c
foreign import primitive prim_MUL_W :: a -> b -> c
foreign import primitive prim_NEG_W :: a -> b
foreign import primitive prim_REM :: a -> b -> c
foreign import primitive prim_QUOT :: a -> b -> c
foreign import primitive prim_SEQ :: a -> b -> c

-- make error primitive
foreign import primitive global_Prelude'_error :: a -> b

-- primitive data types
data Char = Char
data Int = Int
data Integer = Integer
data Float = Float
data Double = Double

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
foreign import primitive global_Prelude'_Prelude'_Integral'_Prelude'_Int'_divMod :: a -> b -> c
foreign import primitive global_Prelude'_Prelude'_Integral'_Prelude'_Integer'_divMod :: a -> b -> c


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

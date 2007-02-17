
-- override things in the Prelude

module Primitive where

import System.IO(Handle)
import Data.Char(ord)


-- local definitions

foreign import primitive any0 :: a
foreign import primitive anyEval1 :: a -> b
foreign import primitive anyEval2 :: a -> b -> c
foreign import primitive anyEval3 :: a -> b -> c -> d

any2 a b = if any0 then a else b
any3 a b c = any2 a (any2 b c)
any4 a b c d = any2 a (any2 b (any2 c d))


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
global_Prelude'_read a b = any2 (error "Prelude.read") (anyEval1 b)
global_Prelude'_Prelude'_Show'_Prelude'_Int'_showsPrec a b c = any0 : anyEval1 b ++ c
global_Prelude'_Prelude'_Show'_Prelude'_Integer'_showsPrec a b c = any0 : anyEval1 b ++ c

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
global_Prelude'_interact f = anyEval1 (f any0)



---------------------------------------------------------------------
-- Data.Char

data Char = Char

-- generates too much data
-- abstractly it generates 1..n characters
global_Data'_Char'_showLitChar :: Char -> ShowS
global_Data'_Char'_showLitChar x y = anyEval1 x : anyEval1 x ++ y

global_Data'_Char'_intToDigit x = anyEval1 x

---------------------------------------------------------------------
-- Data.Array

global_Data'_Array'_array _ x y = anyEval3 x y (error "Data.Array.array is not able to be proven")
global_Data'_Array'_listArray _ x y = anyEval2 x y
global_Data'_Array'_'ex _ x y = anyEval3 x y (error "Data.Array.(!) is not able to be proven")


---------------------------------------------------------------------
-- Numerics

global_Numeric'_showGFloat _ x y = any0 : anyEval1 x ++ y
global_Numeric'_showInt    _ x y = any0 : anyEval1 x ++ y

data Num = Neg | Zero | Pos

not x = case x of {True -> False; False -> True}


divZero x Zero = error "Divide by zero"
divZero x y    = anyEval1 x

numId x = x

numAdd x y = case x of
                Zero -> y
                Pos -> case y of {Neg -> any0; _ -> Pos}
                Neg -> case y of {Pos -> any0; _ -> Neg}

numSub x y = case y of
                Zero -> x
                _ -> any0


numQuot x y = divZero x y
numRem  x y = divZero x y
numDiv  x y = divZero x y
global_Prelude'_Prelude'_Integral'_Prelude'_Int'_mod x y = divZero x y
global_Prelude'_Prelude'__'_divMod _ x y = divZero x y



numEq x y = case x of
                Neg  -> case y of {Neg  -> True; _ -> False}
                Zero -> case y of {Zero -> True; _ -> False}
                Pos  -> case y of {Pos  -> True; _ -> False}

numNe x y = not (numEq x y)


numLt x y = case y of
                Neg  -> case x of {Neg -> any0; _ -> False}
                Zero -> case x of {Neg -> True; _ -> False}
                Pos  -> case x of {Pos -> any0; _ -> True }

numGt x y = anyEval2 x y

global_Prelude'_Prelude'_Num'_Prelude'_Integer'_signum a = a
global_Prelude'_Prelude'_Num'_Prelude'_Integer'_abs a = case a of {Neg -> Pos; _ -> a}

global_Prelude'_gcd x y = case x of
                              Zero -> case y of
                                           Zero -> error "GCD 0 0"
                                           _ -> Pos
                              _ -> Pos


global_Prelude'_'hat _ _ a b = anyEval2 a b
global_Prelude'_'hat'hat _ _ a b = anyEval2 a b
global_Prelude'_Prelude'_Real'_Prelude'_Double'_toRational a = anyEval1 a
global_Prelude'_Prelude'_RealFrac'_Prelude'_Double'_properFraction _ a = anyEval1 a
global_Prelude'_Prelude'_Floating'_Prelude'_Double'_sinh a = anyEval1 a
global_Prelude'_Prelude'_Floating'_Prelude'_Double'_asinh a = anyEval1 a
global_Prelude'_Prelude'_Floating'_Prelude'_Double'_cosh a = anyEval1 a
global_Prelude'_Prelude'_Floating'_Prelude'_Double'_acosh a = anyEval1 a
global_Prelude'_Prelude'_Floating'_Prelude'_Double'_tanh a = anyEval1 a
global_Prelude'_Prelude'_Floating'_Prelude'_Double'_atanh a = anyEval1 a
global_Prelude'_Prelude'___'_atan2 a b c = anyEval3 a b c
global_Prelude'_Prelude'___'_ceiling a b c = anyEval1 c
global_Prelude'_Prelude'___'_exponent _ a = anyEval1 a
global_Prelude'_Prelude'___'_floor _ _ a = anyEval1 a
global_Prelude'_Prelude'___'_logBase _ a b = anyEval2 a b
global_Prelude'_Prelude'___'_round _ _ a = anyEval1 a
global_Prelude'_Prelude'___'_scaleFloat _ a b = anyEval2 a b
global_Prelude'_even _ a = anyEval1 a
global_PreludeAux'__floatFromRational a = anyEval1 a
global_PreludeAux'__doubleFromRational a = anyEval1 a


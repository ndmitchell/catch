
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


---------------------------------------------------------------------
-- Prelude

-- make error primitive
foreign import primitive global_Prelude'_error :: a -> b
global_Prelude'__patternMatchFail c = error c
global_Prelude'__noMethodError c = error c

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
global_Prelude'_lex a = anyEval1 a
global_Prelude'_Prelude'_Show'_Prelude'_Int'_showsPrec a b c = any0 : anyEval1 b ++ c
global_Prelude'_Prelude'_Show'_Prelude'_Integer'_showsPrec a b c = any0 : anyEval1 b ++ c

-- catch/throw stuff - only allow IO errors to be caught
global_Prelude'_catch action handler = any2 action (handler any0)
global_System'_Exit'_exitWith a = anyEval1 a

---------------------------------------------------------------------
-- System.IO


-- ones which have a too concrete implementation
global_System'_IO'_hGetChar x = anyEval1 x
global_System'_IO'_hPutChar x y = anyEval2 x y
global_System'_IO'_stdin  = any0
global_System'_IO'_stdout = any0
global_System'_IO'_stderr = any0
global_System'_IO'_throwIOError x = anyEval1 x
global_System'_IO'_openFile x y = anyEval2 x y
global_System'_IO'_hGetContents x = anyEval1 x
global_System'_IO'_hSetBuffering x y = anyEval2 x y
global_System'_IO'_hFlush x = anyEval1 x
global_System'_Environment'_getEnv x = anyEval1 x
global_System'_Environment'_getArgs = any0
global_System'_Environment'_getProgName = any0


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
global_Data'_Char'_showLitChar x y = anyEval1 x : anyEval1 x ++ y
global_Data'_Char'_lexLitChar x = anyEval1 x
global_Data'_Char'_intToDigit x = anyEval1 x
global_Data'_Char'_isAlpha x = anyEval1 x
global_Data'_Char'_isAlphaNum x = anyEval1 x
global_Data'_Char'_isLower x = anyEval1 x
global_Data'_Char'_isSpace x = anyEval1 x
global_Data'_Char'_isUpper x = anyEval1 x

---------------------------------------------------------------------
-- Data.Array

global_Data'_Array'_array _ x y = anyEval3 x y (error "Data.Array.array is not able to be proven")
global_Data'_Array'_listArray _ x y = anyEval2 x y
global_Data'_Array'_'ex _ x y = anyEval3 x y (error "Data.Array.(!) is not able to be proven")


---------------------------------------------------------------------
-- Numerics

global_Numeric'_showGFloat _ x y = any0 : anyEval1 x ++ y
global_Numeric'_showInt    _ x y = any0 : anyEval1 x ++ y

data Num = Neg | Zero | One | Pos

not x = case x of {True -> False; False -> True}


numId x = x

divZero x Zero = error "Divide by zero"
divZero x y    = anyEval1 x

numQuot x y = case y of
                  Zero -> error "Divide by zero, quotient"
                  One  -> x
                  Pos  -> case x of {One -> Zero; Zero -> Zero; Pos -> any3 Zero One Pos; Neg -> any2 Zero Neg}
                  Neg  -> case x of {One -> any2 Zero Neg; Zero -> Zero; Pos -> any2 Zero Neg; Neg -> any3 Zero One Pos}

numRem  x y = divZero x y
numDiv  x y = divZero x y
numMod  x y = divZero x y

global_Prelude'_Prelude'_Integral'_Prelude'_Int'_mod x y = numMod x y
global_Prelude'_Prelude'__'_divMod _ x y = (numDiv x y, numMod x y)


numAdd x y = case x of
                Zero -> y
                One -> case y of {Neg -> any2 Neg Zero; Zero -> One; One -> Pos; Pos -> Pos}
                Pos -> case y of {Neg -> any0; Zero -> any2 One Pos; One -> Pos; Pos -> Pos}
                Neg -> case y of {Neg -> Neg; Zero -> Neg; One -> any2 Zero Neg; Pos -> any0}

numSub x y = case y of
                Zero -> x
                One -> case y of {Neg -> Neg; Zero -> Neg; One -> Zero; Pos -> any2 One Pos}
                _ -> any0

numMul x y = case x of
                Zero -> Zero
                One -> y
                Pos -> case y of {Neg -> Neg; Zero -> Zero; One -> Pos; Pos -> Pos}
                Neg -> case y of {Neg -> any2 Pos One; Zero -> Zero; One -> Neg; Pos -> Neg}


numEq x y = case x of
                Neg  -> case y of {Neg  -> any0; _ -> False}
                Zero -> case y of {Zero -> True; _ -> False}
                One  -> case y of {One  -> True; _ -> False}
                Pos  -> case y of {Pos  -> any0; _ -> False}

numNe x y = not (numEq x y)


numLt x y = case y of
                Neg  -> case x of {Neg -> any0; _ -> False}
                Zero -> case x of {Neg -> True; _ -> False}
                One  -> case x of {Neg -> True; Zero -> True; _ -> False}
                Pos  -> case x of {Pos -> any0; _ -> True }

numGt x y = case y of
                Neg  -> case x of {Neg -> any0; _ -> True}
                Zero -> case x of {Zero -> False; Neg -> False; _ -> True}
                One  -> case x of {Pos -> True; _ -> False}
                Pos  -> case x of {Pos -> any0; _ -> False}

global_Prelude'_Prelude'_Num'_Prelude'_Integer'_signum a = case a of {Pos -> One; _ -> a}

global_Prelude'_Prelude'_Num'_Prelude'_Integer'_abs a = case a of {Neg -> any2 One Pos; _ -> a}

global_Prelude'_gcd _ x y = case x of
                                Zero -> case y of
                                            Zero -> error "GCD 0 0"
                                            _ -> any2 One Pos
                                _ -> any2 One Pos


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



---------------------------------------------------------------------
-- Data.Ratio

-- not correct, not free enough
-- if anyone ever tries to use ratio on their own types, will fail
-- unlikely though
data Global_Ratio a = Global_Data'_Ratio'_'col'per Num Num


global_Data'_Ratio'_'per _ a b =
    case b of
        Zero -> error "Data.Ratio.% with zero as the second argument, bad person"
        One  -> Global_Data'_Ratio'_'col'per a b
        _    -> Global_Data'_Ratio'_'col'per (any3 Neg One Pos) (any2 One Pos)

global_Data'_Ratio'_reduce a b c = global_Data'_Ratio'_'per a b c

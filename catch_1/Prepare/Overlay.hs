
module Prepare.Overlay(overlay) where

import System.Cmd
import System.Exit
import Data.Maybe
import Control.Monad
import Yhc.Core
import General.General
import System.FilePath



overlay :: Core -> IO Core
overlay core = do
    base <- baseDir
    res <- system $ "yhc -hide -core " ++ base </> "examples/Library/Primitive.hs"
    when (res /= ExitSuccess) $ error "Failed to compile the overlay"

    over <- loadCore $ base </> "examples/Library/ycr/Primitive.ycr"
    return $ coreReachable ["main"] $ coreOverlay (abstract core) over


abstract = litAbstract . primAbstract . caseAbstract


---------------------------------------------------------------------
-- Remove cases on constants
caseAbstract :: Core -> Core
caseAbstract core = mapUnderCore f core
    where
        f (CoreCase on alts) | any (isCoreChr . fst) alts
            = CoreCase on [(CoreApp (CoreCon "Primitive.Char") [], anys (map snd alts))]

        f x@(CoreCase on alts) | any (isCoreConst . fst) alts
            = CoreCase on (cas "Neg" neg ++ cas "Zero" zero ++ cas "One" one ++ cas "Pos" pos ++ def)
            where
                zero = let x = pick "Zero" in if null x then other else x
                one  = let x = pick "One"  in if null x then other else x
                pos = other ++ pick "Pos"
                neg = other ++ pick "Neg"
                other = pick ""
            
                pick x = map snd $ filter ((==) x . g . fst) alts
                g x = if isCoreConst x then constAbstract x else ""
                
                def = [(CoreVar "_", anys other) | any (== other) [neg, zero, one, pos]]
                cas x rs = [(CoreApp (CoreCon ("Primitive." ++ x)) [], anys rs) | rs /= other]

        f x = x
        
        
        anys [x] = x
        anys (x:xs) = CoreApp (CoreFun "Primitive.any2") [x, anys xs]
   

---------------------------------------------------------------------
-- Remove cases on constants

{-
-- NEW VERSION USING Yhc.Core.Prim
-- causes regressions in Nofib

numPrims = [(PrimAdd, "numAdd"), (PrimSub, "numSub"), (PrimMul, "numMul")
           ,(PrimDiv, "numDiv"), (PrimQuot, "numQuot"), (PrimRem, "numRem")
           ,(PrimEq, "numEq"), (PrimNe, "numNe"), (PrimLt, "numLt"), (PrimGt, "numGt")
           ,(PrimCast, "numId")
           ]


primAbstract :: Core -> Core
primAbstract = mapUnderCore f
    where
        f (CoreFun  x) = g CoreFun  x
        f (CorePrim x) = g CorePrim x
        f x = x
        
        g rebuild x = fromMaybe (rebuild x) $ do
                          prim <- corePrimMaybe x
                          func <- lookup (primOp prim) numPrims
                          return $ CoreFun $ "Primitive." ++ func
-}

numPrims = [("ADD_W","numAdd"),("SUB_W","numSub"),("MUL_W","numMul")
           ,("LT_W","numLt"),("GT_W","numGt")
           ,("QUOT","numQuot"),("REM","numRem"),("SLASH_D","numDiv")
           ,("YHC.Primitive.primIntegerEq","numEq")
           ,("YHC.Primitive.primIntegerQuot","numQuot")
           ,("YHC.Primitive.primDoubleFromInteger","numId")
           ,("YHC.Primitive.primIntegerFromInt","numId")
           ,("YHC.Primitive.primIntegerAdd","numAdd")
           ,("YHC.Primitive.primIntegerMul","numMul")
           ,("YHC.Primitive.primIntegerNe","numNe")
           ]


primAbstract :: Core -> Core
primAbstract = mapUnderCore f
    where
        f (CoreFun  x) = g CoreFun  x
        f (CorePrim x) = g CorePrim x
        f x = x
        
        g rebuild x = case lookup x numPrims of
                           Nothing -> rebuild x
                           Just y -> CoreFun ("Primitive." ++ y)



litAbstract :: Core -> Core
litAbstract = mapUnderCore f
    where
        f x | isPrimConst x = CoreApp (CoreCon ("Primitive." ++ constAbstract x)) []
        f x = x


isPrimConst x = isCoreConst x && not (isCoreStr x)


constAbstract :: CoreExpr -> String
constAbstract x = f x
    where
        f (CoreChr     x) = "Char"
        f (CoreInt     x) = number x
        f (CoreInteger x) = number x
        f (CoreDouble  x) = number x
        f (CoreFloat   x) = number x

        number x = if x < 0 then "Neg"
                   else if x == 0 then "Zero"
                   else if x == 1 then "One"
                   else "Pos"

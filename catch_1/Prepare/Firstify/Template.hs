
module Prepare.Firstify.Template(Template, genTemplate, useTemplate, isHO) where

import Yhc.Core hiding (collectAllVars,collectFreeVars,uniqueBoundVars,replaceFreeVars)
import Yhc.Core.FreeVar2

import Data.List
import Control.Monad

{-
SPECIALISE ALGORITHM

Need to generate a specialised version if:
* f gets called with more arguments than its arity
* any argument is higher order

The specialised version has:
* a free variable for each non-ho argument
* the free variables within a function, for a ho argument
-}

isHO :: CoreExpr -> Bool
isHO (CoreLet _ x) = isHO x
isHO (CoreLam _ _) = True
isHO (CoreCase x ys) = any (isHO . snd) ys
isHO (CoreApp (CoreCon _) args) = any isHO args
isHO _ = False


data Template = Template [CoreExpr]
                deriving (Eq,Ord)


-- given a call to this function, with the given arguments
-- return Just (Template, [Args]) if you want to make it a new call
useTemplate :: CoreFunc -> [CoreExpr] -> Maybe (Template, [CoreExpr])
useTemplate func xs | nxs >= ar && (nxs > ar || any isHO xs)
        = Just (Template ts, concat xs2)
    where
        nxs = length xs
        ar = length $ coreFuncArgs func
        
        (ts,xs2) = unzip $ map f xs

        f x | isHO x    = (normVars x  , map CoreVar $ collectFreeVars x)
            | otherwise = (CoreVar "v1", [x])

useTemplate _ _ = Nothing



genTemplate :: Template -> CoreFunc -> CoreFuncName -> CoreFunc
genTemplate (Template xs) (CoreFunc _ args body) newname = CoreFunc newname (concat args2) body2
    where
        (norm,extra) = splitAt (length args) reps
        (args2,reps) = runFreeVars $ do deleteVars (concatMap collectAllVars (body:xs))
                                        mapAndUnzipM f xs
        
        body2 = coreApp (replaceFreeVars (zip args norm) body) extra

        -- return the arguments you require, and the expression you are    
        f :: CoreExpr -> FreeVar ([CoreVarName], CoreExpr)
        f x = do
            let free = collectFreeVars x
            vs <- replicateM (length $ collectFreeVars x) getVar
            return (vs, replaceFreeVars (zip free $ map CoreVar vs) x)



-- for all equivalent expressions
-- irrelevant of free or bound var names, alpha rename to the same thing
normVars :: CoreExpr -> CoreExpr
normVars x = normFree 'f' [] $ normBound 'b' [] $ normFree 'F' seen $ normBound 'B' seen x
    where
        seen = collectAllVars x

        normBound c seen x = runFreeVars $ do
            putVars (freeVars c \\ seen)
            uniqueBoundVars x

        normFree c seen x = replaceFreeVars
            (zip (collectFreeVars x) (map CoreVar $ freeVars c \\ seen))
            x


module Hill.Simple(cmdsSimple, simplify, simplifyEx, SimplifyOpt(..), normalise, applyFuns, useVectorMake) where

import Hill.Type
import Data.List
import Data.Maybe
import General.General


cmdsSimple =
    [hillCmdPure "simplify" (\name hill -> simplify hill hill)
    ,hillCmdPure "normalise" (const normalise)
    ,hillCmdPure "apply-funs" (const applyFuns)
    ,hillCmdPure "simple-inline" (const simpleInline)
    ,hillCmdPure "vector" (const useVector)
    ,hillCmdPure "novector" (const noVector)
    ]


---------------------------------------------------------------------

data SimplifyOpt = NoLet deriving Eq

-- basic simplifications
simplify :: ManipulateHill hill => Hill -> hill -> hill
simplify = simplifyEx []

simplifyEx :: ManipulateHill hill => [SimplifyOpt] -> Hill -> hill -> hill
simplifyEx opts hill x = mapOverHill f x
    where
        yesLet = not $ NoLet `elem` opts
    
        -- use error if you can
        f (Apply (Fun "error") [x]) = Error x
        f (Call "error" [x]) = Error x
        
        -- inline simple lets, @1 = @2
        f (Let binds x) | yesLet && not (null simp) = f $ mkLet complex $ mapOverHill g x
            where
                g (Var x) = case lookup x simp of
                                Nothing -> Var x
                                Just y -> y
                g x = x
            
                (simp, complex) = partition (isVar . snd) binds
        
        -- discard unused lets
        f (Let binds x) | yesLet && not (null unused) = f $ mkLet used x
            where
                required = requiredFree x
                (used, unused) = partition ((`elem` required) . fst) binds
        
        -- float chains of lets into one (if possible)
        f (Let binds1 (Let binds2 x)) | yesLet && not (null float) = Let (binds1++float) (mkLet nofloat x)
            where
                binded = map fst binds1
                (float, nofloat) = partition (disjoint binded . usedFree . snd) binds2
        
        -- let x = blah in x
        f (Let binds (Var x)) | yesLet && isJust y = fromJust y
            where y = lookup x binds
        
        -- case Ctor of Ctor -> x ==> x
        f (Case on alts) | isJust onConst && isJust alt = fromJust alt
            where
                onConst = getConst on
                alt = listToMaybe [y | Alt x y <- alts, x == fromJust onConst]
            
                getConst (Make x xs) = Just $ ACtor x
                getConst (Apply x _) = getConst x
                getConst (Const x) = Just x
                getConst _ = Nothing
        
        -- (Ctor x y).sel1 ==> x
        f (Sel on path) | isJust res = fromJust res
            where
                carg = getCArg hill path
                (ctor,pos) = (ctorName carg, cargPos carg)
                
                res = g on
                g (Apply (Const (ACtor name)) args) = g (Make name args)
                g (Make name args) | name == ctor && length args > pos = Just (args !! pos)
                g _ = Nothing
        
        f x = x


---------------------------------------------------------------------

-- very simple inlining
-- if the result is a constant, or a forwarding call
simpleInline hill = mapOverHill f hill
    where
        f orig@(Call x xs) = checkInline orig x xs
        f orig@(Apply (Fun x) xs) = checkInline orig x xs
        f x = x
        
        
        checkInline orig name args =
            case lookup name inliners of
                Just (Func _ params body) | length args == length params ->
                    f $ replaceFree (zip params args) body
                _ -> orig

    
        inliners = map (\a -> (funcName a, a)) $ filter canInline $ funcs hill
    
        canInline (Func nam _ (Apply (Fun name) args)) | name /= nam = all isVar args
        canInline (Func nam _ (Call name args)) | name /= nam = all isVar args
        canInline x = canInlineExpr $ body x
        
        canInlineExpr (Const _) = True
        canInlineExpr (Var _) = True
        canInlineExpr (Prim name args) = all isVar args
        canInlineExpr _ = False


---------------------------------------------------------------------


normalise :: ManipulateHill hill => hill -> hill
normalise hill = mapOverHill f hill
    where
        f (Apply x xs) = mkApply x xs
        f (Let xs x) = mkLet xs x
        f (Lambda n x) = mkLambda n x
        f x = x


applyFuns :: ManipulateHill hill => hill -> hill
applyFuns hill = mapOverHill f hill
    where
        f (Fun x) = Apply (Fun x) []
        f (Apply (Apply (Fun x) []) xs) = Apply (Fun x) xs
        f x = x


---------------------------------------------------------------------


useVector hill = mapOverHill f hill
    where
        f (Apply (Fun x) xs)
                | length (funcArgs func) == length xs 
                = Call x xs
            where func = getFunc hill x
        
        f (Fun x)
                | null $ funcArgs $ getFunc hill x
                = Call x []
        
        f (Apply (Const (ACtor x)) xs)
                | length (ctorArgs ctor) == length xs
                = Make x xs
            where ctor = getCtor hill x
        
        f (Const (ACtor x))
                | null $ ctorArgs $ getCtor hill x
                = Make x []

        f x = x


useVectorMake :: ManipulateHill hill => hill -> hill
useVectorMake x = mapOverHill f x
    where
        f (Apply (Const (ACtor x)) xs) = Make x xs
        f x = x


noVector hill = mapOverHill f hill
    where
        f (Call x xs) = Apply (Fun x) xs
        f (Make x xs) = Apply (Const (ACtor x)) xs
        f x = x

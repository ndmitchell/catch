
module Hill.Simple(cmdsSimple) where

import Hill.Type
import Data.List


cmdsSimple =
    [hillCmdPure "simplify" (const simplify)
    ,hillCmdPure "normalise" (const normalise)
    ,hillCmdPure "simple-inline" (const simpleInline)
    ,hillCmdPure "vector" (const useVector)
    ,hillCmdPure "novector" (const noVector)
    ]


---------------------------------------------------------------------

-- basic simplifications
simplify :: Hill -> Hill
simplify hill = mapOverHill f hill
    where
        -- use error if you can
        f (Apply (Fun "error") [x]) = Error x
        f (Call "error" [x]) = Error x
        
        -- inline simple lets, @1 = @2
        f (Let binds x) | not (null simp) = mkLet complex $ mapOverHill g x
            where
                g (Var x) = case lookup x simp of
                                Nothing -> Var x
                                Just y -> y
                g x = x
            
                (simp, complex) = partition (isVar . snd) binds
        
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


normalise hill = mapOverHill f hill
    where
        f (Apply x xs) = mkApply x xs
        f (Let xs x) = mkLet xs x
        f (Lambda n x) = mkLambda n x
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


noVector hill = mapOverHill f hill
    where
        f (Call x xs) = Apply (Fun x) xs
        f (Make x xs) = Apply (Const (ACtor x)) xs
        f x = x

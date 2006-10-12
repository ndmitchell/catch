
module Hill.Simple(cmdsSimple) where

import Hill.Type
import Data.List


cmdsSimple =
    [hillCmdPure "simplify" (const simplify)
    ,hillCmdPure "simple-inline" (const simpleInline)
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
                    replaceFree (zip params args) body
                Nothing -> orig

    
        inliners = map (\a -> (funcName a, a)) $ filter canInline $ funcs hill
    
        canInline (Func _ _ (Const a)) = True
        canInline (Func nam _ (Apply (Fun name) args)) | name /= nam = all isVar args
        canInline (Func nam _ (Call name args)) | name /= nam = all isVar args
        canInline _ = False
        
        
        
        
        
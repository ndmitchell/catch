
module Hill.Inline(cmdsInline) where

import Hill.Type
import General.General


cmdsInline = [hillCmdPure "simple-inline" (const simpleInline)]


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
    
        canInline (Func nam _ (Apply (Fun name) args)) | name /= nam = uniqueVars args
        canInline (Func nam _ (Call name args)) | name /= nam = uniqueVars args
        canInline x = canInlineExpr $ body x
        
        uniqueVars xs = all isVar xs && length xs == length (snub [x | Var x <- xs])
        
        canInlineExpr (Const _) = True
        canInlineExpr (Sel x _) = canInlineExpr x
        canInlineExpr (Var _) = True
        canInlineExpr (Prim name args) = uniqueVars args
        canInlineExpr _ = False


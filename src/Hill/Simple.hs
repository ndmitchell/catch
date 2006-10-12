
module Hill.Simple(cmdsSimple) where

import Hill.Type
import Data.List


cmdsSimple =
    [hillCmdPure "simplify" (const simplify)
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

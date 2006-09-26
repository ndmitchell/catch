
-- If two constructors are indistinguishable in the code
-- combine them into one

module Hite.CtorCollapse2(cmd) where

import Data.List
import Hite.Type
import General.General


cmd = cmdHitePure (const ctorCollapse2) "ctor-collapse2"
            "Collapse constructors which are equivalent for most purposes"


ctorCollapse2 :: Hite -> Hite
ctorCollapse2 hite@(Hite datas funcs) = Hite (map mergeData datas) newfuncs
    where
        newfuncs = mapExpr mergeFunc funcs
        newdatas = [take (length x - 2) x | Make x xs <- allExpr newfuncs, last x == '*']
    
        usefulCtors = nub [a | Case on alts <- allExpr hite, (a,_) <- alts, a /= ""]
        
        mergeFunc (Make x [])
            | not $ x `elem` usefulCtors
            = Make (dataName (getCtor hite x) ++ "_*") []
        mergeFunc x = x
        
        mergeData (Data nam ctrs t) = Data nam 
            ([Ctor (nam ++ "_*") [] [] | nam `elem` newdatas] ++ [c | c@(Ctor name _ _) <- ctrs, name `elem` usefulCtors])
            t

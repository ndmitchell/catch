
-- If two constructors are indistinguishable in the code
-- combine them into one

module Hite.CtorCollapse(cmd) where

import Data.List
import Hite.Type
import General.General


cmd = cmdHitePure (const ctorCollapse) "ctor-collapse"
            "Collapse constructors which are equivalent for most purposes"


ctorCollapse :: Hite -> Hite
ctorCollapse hite@(Hite datas funcs) = Hite (map mergeData datas) (mapExpr mergeFunc funcs)
    where
        combine = pickCombine hite
        
        shorten x | length x > 20 = take 20 x ++ "+++"
                  | otherwise = x
        
        getName x = case [y | y <- combine, x `elem` y] of
                        [x] -> shorten $ intercat "+" x
                        _ -> x
        
        mergeData dat = dat{ctors = nubExtract ctorName $ map f $ ctors dat}
            where f ctr = ctr{ctorName = getName $ ctorName ctr}

        mergeFunc (Make x xs) = Make (getName x) xs
        mergeFunc (Case on alts) = Case on $ nubExtract fst $ map f alts
            where f (ctr,expr) = (getName ctr, expr)
        mergeFunc x = x


-- all the ctors in a given set can be combined
pickCombine :: Hite -> [[CtorName]]
pickCombine hite = filter (not . singleton) $ splitDiffs $ splitUsed initial
    where
        initial = map (map ctorName . ctors) $ datas hite
        used = ctorsUsed hite
        diffs = ctorDiffs hite
        
        splitUsed xs = concatMap f xs
            where
                f x = b : map (:[]) a
                    where (a,b) = partition (`elem` used) x
        
        splitDiffs xs = concatMap f xs
            where
                f x = foldl g [x] diffs
                
                g x (d1,d2) = concatMap h x
                    where
                        h y | d1 `elem` y && d2 `elem` y = [[d1],filter (/= d1) y]
                            | otherwise = [y]


-- for every case statements where two variables disagree, mark them
ctorDiffs :: Hite -> [(CtorName, CtorName)]
ctorDiffs hite = concatMap f $ allExpr hite
    where
        f (Case on alts) = concatMap (g alts) alts
        f x = []
        
        g alts (name,expr) = map (((,) name) . fst) $ filter ((/=) expr . snd) alts


-- if a constructor is used as a literal, you can't collapse it
ctorsUsed :: Hite -> [CtorName]
ctorsUsed hite = nub [x | Make x _ <- allExpr hite]


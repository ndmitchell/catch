
module Reqs.Simplify(simplifyReqs, simplifyReqsFull) where

import Hite

import Reqs.Type
import Reqs.Path
import Reqs.Show
import Pred.Type
import Pred.Simplify
import Pred.Form

import General.Simplify
import General.General
import Maybe
import List

import Star.Type



simplifyReqsFull :: Hite -> Reqs -> Reqs
simplifyReqsFull hite x = error $ prettyReqs $ dnf x



simplifyReqs :: Bool -> Hite -> Reqs -> Reqs
simplifyReqs b hite x = (if b then simplifyPredFull else simplifyPred)
        [Rule ruleSingleOr , RuleAssoc ruleSubsetOr, RuleAssoc ruleImplies]
        [Rule ruleSingleAnd, Rule ruleSameCondAnd, RuleAssoc ruleImplies2]
        [RuleOne ruleDel]
        x
    where
        {-
        ruleAnd1 (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && c1 == c2
            = Just (Req a1 (regUnion [b1,b2]) c1)
        ruleAnd1 _ _ = Nothing
        
        ruleAnd2 (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && b1 == b2 && isFinite b1
            = Just (Req a1 b1 (c1 `intersect` c2))
        ruleAnd2 _ _ = Nothing

        
        ruleOr1 (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && isFinite b1 && isJust res &&
              not (isEwp $ fromJust res) && null (c1 `intersect` ctors)
            = Just (Req a2 b2 c2)
            where
                res = dropPrefix b1 b2
                hds = nextChar (fromJust res)
                ctors = nub $ map (ctorName . (`getCtorFromArg` hite)) hds
        ruleOr1 _ _ = Nothing
        
        
        ruleOr2 (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && b1 == b2 && isFinite b1
            = Just (Req a1 b1 (nub $ c1 ++ c2))
        ruleOr2 _ _ = Nothing
        -}
        
        {-
        -- VERY WRONG
        ruleSameCondOr (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && c1 `setEq` c2
            = Just (Req a1 (b1 `pathIntersect` b2) c1)
        ruleSameCondOr _ _ = Nothing
        -}
        
        ruleSameCondAnd (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && c1 `setEq` c2
            = Just (Req a1 (b1 `pathUnion` b2) c1)
        ruleSameCondAnd _ _ = Nothing
        
        ruleSubsetOr (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && b1 `pathSubset` b2 && c1 `setEq` c2
            = Just (Req a1 b1 c1)
        ruleSubsetOr _ _ = Nothing
        
        ruleSingleOr (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && b1 == b2 && pathIsSingle b1
            = Just (Req a1 b1 (c1 `union` c2))
        ruleSingleOr _ _ = Nothing
        
        
        ruleSingleAnd (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && b1 == b2 && pathIsSingle b1
            = Just (Req a1 b1 (c1 `intersect` c2))
        ruleSingleAnd _ _ = Nothing
        
        
        ruleDel (PredLit (Req a1 b1 c1))
            | null c1
            = Just predFalse -- not true, what if none defined - to fix
        ruleDel (PredLit (Req a1 b1 c1))
            | (map ctorName $ ctors $ getDataFromCtor (head c1) hite) `setEq` c1
            = Just predTrue
        ruleDel (PredLit (Req a1 b1 c1))
            | pathIsOmega b1
            = Just predTrue
        ruleDel _ = Nothing
        

        ruleImplies (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && (b1 `elem` diffs)
            = Just $ Req a2 b2 c2
            where
                diffs = f [] (map (`rdiff` b2) iargs)
                ictors = (map ctorName $ ctors $ getDataFromCtor (head c1) hite) \\ c1
                iargs = concatMap (\x -> ctorArgs $ getCtor x hite) ictors

                rdiff x reg = pathReverse (pathQuotient x (pathReverse reg))

                f done [] = done
                f done (t:odo) | t `elem` done = f done odo
                               | otherwise = f (t:done) (t2 ++ odo)
                    where t2 = map (`rdiff` t) iargs
        ruleImplies _ _ = Nothing


        ruleImplies2 (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && (b1 `pathSubset` b2) && (c1 == (c1 \\ c2))
            = Just (Req a1 b1 [])
        ruleImplies2 _ _ = Nothing

{-
        ruleImplies r1@(Req a1 b1 c1) r2@(Req a2 b2 c2)
            | b1 == Lambda && c1 == ["[]"]
            = error $ show ("here", r1, r2, iargs, diffs)
            where
                diffs = f [] (map (`rdiff` b2) iargs)
                ictors = (map ctorName $ ctors $ getDataFromCtor (head c1) hite) \\ c1
                iargs = concatMap (\x -> ctorArgs $ getCtor x hite) ictors

                rdiff x reg = pathReverse (pathQuotient x (pathReverse reg))

                f done [] = done
                f done (t:odo) | t `elem` done = f done odo
                               | otherwise = f (t:done) (t2 ++ odo)
                    where t2 = map (`rdiff` t) iargs
-}

{-
dropPrefix :: (Eq a, Show a) => RegExp a -> RegExp a -> Maybe (RegExp a)
dropPrefix a b = f (fromConcat a) (fromConcat b)
    where
        f (a:as) (b:bs) | a == b = f as bs
        f [] xs = Just $ regConcat xs
        f _ _ = Nothing
-}


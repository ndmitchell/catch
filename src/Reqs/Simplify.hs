
module Reqs.Simplify(simplifyReqs, simplifyReq) where

import Hite

import Reqs.Type
import Reqs.Path
import Pred.Type
import Pred.Simplify

import General.Simplify
import General.General
import Maybe
import List


simplifyReqs :: Hite -> Reqs -> Reqs
simplifyReqs hite x = simplifyPred
        [Rule ruleSingleOr ]
        [Rule ruleSingleAnd]
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
            = Just predFalse
        ruleDel (PredLit (Req a1 b1 c1))
            | (map ctorName $ ctors $ getDataFromCtor (head c1) hite) `setEq` c1
            = Just predTrue
        ruleDel _ = Nothing
        

{-
dropPrefix :: (Eq a, Show a) => RegExp a -> RegExp a -> Maybe (RegExp a)
dropPrefix a b = f (fromConcat a) (fromConcat b)
    where
        f (a:as) (b:bs) | a == b = f as bs
        f [] xs = Just $ regConcat xs
        f _ _ = Nothing
-}


simplifyReq x = x


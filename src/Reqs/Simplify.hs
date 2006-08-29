
module Reqs.Simplify(simplifyReqs, simplifyReqsFull, simplifyReqAllsFull, simpMore) where

import Hite

import Reqs.Type
import Reqs.Path
import Reqs.Show
--import Pred.Type
--import Pred.Simplify
--import Pred.Form

import General.Simplify
import General.General
import Maybe
import List

import Star.Type

import Data.Predicate



-- * Super Simplify Section

data Wrap = Wrap {unwrap :: Req} deriving Eq

instance PredLit Wrap where
    a ?=> b = unwrap a $=> unwrap b
    a ?\/ b = rewrap $ unwrap a $\/ unwrap b
    a ?/\ b = case unwrap a $/\ unwrap b of
                  Same -> rewrap $ unwrap a $!/\ unwrap b
                  x -> rewrap x
    simp a = simpReq (unwrap a)


rewrap (Single a) = Single (Wrap a)
rewrap (Priority x a) = Priority x (Wrap a)
rewrap Same = Same
rewrap (Value x) = Value x


simpMore :: ReqAlls -> ReqAlls
simpMore x = mapPredLit f x
    where
        f (ReqAll a b) = predLit $ ReqAll a $ mapPredLit (predLit . unwrap) $ mapPredLit (predLit . Wrap) b


(Req a1 b1 c1 hite) $!/\ (Req a2 b2 c2 _)
    | a1 == a2 && c1 `setEq` c2 && isJust b0
        = Single $ Req a1 (fromJust b0) c1 hite
    | otherwise = Same
        where b0 = b1 `pathUnion` b2





-- * Standard Simplify Section

instance PredLit ReqAll where
    (ReqAll a1 b1) ?/\ (ReqAll a2 b2) | a1 == a2 = Single $ ReqAll a1 (predAnd [b1, b2])
                                      | otherwise = Same


instance PredLitNot Req where
    litNot (Req on path set hite) = predLit $ Req on path (ctorNames (getCtor hite (head set)) \\ set) hite


instance PredLit Req where
    (?=>) = ($=>)
    (?\/) = ($\/)
    (?/\) = ($/\)

    simp = simpReq
    
    

simpReq (Req a1 b1 c1 hite)
    | c1 `setEq` (ctorNames $ getCtor hite (head c1))
        = Just True
    | otherwise
        = Nothing

(Req a1 b1 c1 hite) $\/ (Req a2 b2 c2 _) 
    | a1 == a2 && b1 == b2 && pathIsFinite b1
        = Single $ Req a1 b1 (c1 `union` c2) hite
    | otherwise = Same

_ $/\ _ = Same


($=>) :: Req -> Req -> Bool
(Req a1 b1 c1 hite) $=> (Req a2 b2 c2 _)
    | a1 /= a2 = False
    | b2 `pathSubset` b1 && null (c1 \\ c2) = True
    | b1 == b2 && c1 `setEq` c2 = True -- should be redundant, if pathSubset is == implies
    | pathIsFinite b1 && superImply hite b1 c1 b2 = True
    | otherwise = False

superImply :: Hite -> Path String -> [CtorName] -> Path String -> Bool
superImply hite b1 c1 b2 = all (\l2 -> any (f l2) b1s) b2s
    where
        b1s = pathEnumerate b1
        b2s = pathEnumerate (pathMakeFinite b2)

        f l2 l1 = length l1 < length l2 &&
                  l1 `isPrefixOf` l2 &&
                  not (ctorName ctr `elem` c1)
            where
                c = headNote "Reqs.Simplify.superImply" (drop (length l1) l2)
                ctr = getCArg hite c




-- simplify all the forall statements
simplifyForall :: ReqAlls -> ReqAlls
simplifyForall x = x {- predOr $ map f $ fromOr $ dnf x
    where
        f x = predAnd $ map g $ groupBy (\a b -> getName a == getName b) $ fromAnd x
        
        g xs | nx == ""  = predAnd xs
             | otherwise = predLit $ ReqAll nx $ predAnd $ map (reqWithin . fromPredLit) xs
            where nx = getName $ headNote "Reqs.Simplify.simplifyForall" xs
        
        getName (PredLit (ReqAll a b)) = a
        getName _ = ""  -}


simplifyReqAllsFull :: Hite -> ReqAlls -> ReqAlls
simplifyReqAllsFull hite x = x {- mapPredLit simpOne $ simplifyForall x
    where
        simpOne (ReqAll name x) | isTrue x = predTrue
                                | otherwise = predLit $ ReqAll name (simplifyReqsFull hite x) -}
    


-- Must all be Nothing as their within property
simplifyReqsFull :: Hite -> Reqs -> Reqs
simplifyReqsFull hite x = x {- simplifyReq x
    where

        anyForall x = not $ null [() | ReqAll _ _ <- allPredLit x]
    
        simplifyReq x =
            andPairs finalMerge $
            orPairsAs orSubsetCollapse $
            mapPredLit atomNullCtors $
            andPairs andEqPathCollapse $
            orPairs orEqPathCollapse $
            andPairsAs andSubsetCollapse $
            mapPredLit addFinite $
            dnf x { - $
            andPairs finalMerge $
            orPairsAs orSubsetCollapse $
            mapPredLit atomNullCtors $
            andPairs andEqPathCollapse $
            orPairs orEqPathCollapse $
            andPairsAs andSubsetCollapse $
            mapPredLit addFinite $
            cnf x - }

        
        andPairsAs f xs = andPairs (makeAssoc f) xs
        orPairsAs  f xs = orPairs  (makeAssoc f) xs

        andPairs :: (Req -> Req -> Maybe Req) -> Reqs -> Reqs
        andPairs g xs = mapPred f xs
            where
                f (PredAnd xs) = predAnd $ nots ++ map predLit (simplifySet [Rule g] (map fromPredLit lits))
                    where (lits, nots) = partition isPredLit xs
                f x = x
        
        
        orPairs :: ([Req] -> [Req] -> Maybe [Req]) -> Reqs -> Reqs
        orPairs g xs = mapPred f xs
            where
                f (PredOr xs) | all isPredLit (concat items)
                              = predOr $ map (predAnd . map predLit) $ simplifySet [Rule g] $ map (map fromPredLit) items
                    where items = map fromAnd xs
                f x = x
        
        
        addFinite (Req a b c d)
            | not (pathIsFinite b)
            = predAnd [predLit $ Req a b c d, predLit $ Req a (pathMakeFinite b) c d]
        addFinite x = predLit x
        
        
        andSubsetCollapse a b | a ==> b = Just a
        andSubsetCollapse _ _ = Nothing
        
        orSubsetCollapse as bs | all (\a -> any (\b -> b ==> a) bs) as = Just as
        orSubsetCollapse _ _ = Nothing


        andEqPathCollapse (Req a1 b1 c1 d1) (Req a2 b2 c2 d2)
            | a1 == a2 && b1 == b2
            = Just $ Req a1 b1 (c1 `intersect` c2) d1
        andEqPathCollapse _ _ = Nothing
        
        

        -- if the set is null, then try and prove the path is not reachable
        -- by following the quotients
        atomNullCtors (Req a1 b1 c1 d1)
            | null c1
            = if pathIsEwp b1 then predFalse else res
                where
                    res = predAnd [f (map ctorName ctrs \\ [cn]) alt |
                              Data dn ctrs <- datas hite, Ctor cn alts <- ctrs, alt <- alts]
                
                    f cns an = if pathIsOmega b2 then predTrue
                               else predLit (Req a1 b2 cns d1)
                        where b2 = an `pathQuotient` b1
        atomNullCtors (Req a1 b1 c1 d1)
            | c1 `setEq` (getCtorsFromCtor hite (headNote "Reqs.Simplify.atomNullCtors" c1))
            = predTrue
        atomNullCtors x = predLit x
        
        
        finalMerge (Req a1 b1 c1 d1) (Req a2 b2 c2 d2)
            | a1 == a2 && c1 `setEq` c2
            = Just $ Req a1 (b1 `pathUnion` b2) c1 d1
        finalMerge _ _ = Nothing
        
        

        -- does a imply b
        (==>) :: Req -> Req -> Bool
        (Req a1 b1 c1 d1) ==> (Req a2 b2 c2 d2)
            | a1 /= a2 = False
            | b2 `pathSubset` b1 && null (c1 \\ c2) = True
            | b1 == b2 && c1 `setEq` c2 = True -- should be redundant, if pathSubset is == implies
            | pathIsFinite b1 && superImply b1 c1 b2 = True
            | otherwise = False
            
        
        superImply :: Path String -> [CtorName] -> Path String -> Bool
        superImply b1 c1 b2 = all (\l2 -> any (f l2) b1s) b2s
            where
                b1s = pathEnumerate b1
                b2s = pathEnumerate (pathMakeFinite b2)
                
                f l2 l1 = length l1 < length l2 &&
                          l1 `isPrefixOf` l2 &&
                          not (ctorName ctr `elem` c1)
                    where
                        c = headNote "Reqs.Simplify.superImply" (drop (length l1) l2)
                        ctr = getCtorFromArg hite c
                
            
            -}
        
        
        
        {-
        -- which regular expressions are sufficient to imply b
        implySet :: Req -> [Req]
        implySet 
        
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
-}
        
{-        
        
        
        
        
        rOr = [Rule ruleOrAs]
        rAnd = [Rule ruleAnd, RuleAssoc ruleAndAs]
        
        
        ruleAnd (PredLit (Req a1 b1 c1)) (PredLit (Req a2 b2 c2))
            | a1 == a2 && b1 == b2
            = if null c3
            then Just $ predAnd [f (map ctorName ctrs \\ [cn]) alt |
                                 Data dn ctrs <- datas hite, Ctor cn alts <- ctrs, alt <- alts]
            else Just $ PredLit $ Req a1 b1 c3
                where
                    c3 = c1 `intersect` c2
                    
                    dns = [x | Data x _ <- datas hite]
                    f cns an = if pathIsOmega b3 then predTrue
                               else predLit (Req a1 b3 cns)
                        where b3 = an `pathQuotient` b1
        ruleAnd _ _ = Nothing
        
        
        ruleAndAs (PredLit (Req a1 b1 c1)) (PredLit (Req a2 b2 c2))
            | a1 == a2 && c1 `setEq` c2 && b1 `pathSubset` b2
            = Just $ PredLit $ Req a1 b2 c2
        
        ruleAndAs _ _ = Nothing
        
        
        ruleOrAs xs ys 
            | allIsLit xs2 && allIsLit ys2 && all f xs2
            = Just xs
            where
                (xs2, ys2) = (fromAnd xs, fromAnd ys)
                f (PredLit (Req a1 b1 c1)) = or [b1 `pathSubset` b2 && a1 == a2 && c1 == c2 | PredLit (Req a2 b2 c2) <- ys2]
            
        ruleOrAs _ _ = Nothing
        
        
        allIsLit x = all isPredLit x
-}



simplifyReqs :: Bool -> Hite -> Reqs -> Reqs
simplifyReqs b hite x = x {- (if b then simplifyPredFull else simplifyPred)
        [Rule ruleSingleOr , RuleAssoc ruleSubsetOr, RuleAssoc ruleImplies]
        [Rule ruleSingleAnd, Rule ruleSameCondAnd, RuleAssoc ruleImplies2]
        [RuleOne ruleDel]
        x
    where -}
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
        
        {-
        ruleSameCondAnd (Req a1 b1 c1 d1) (Req a2 b2 c2 d2)
            | a1 == a2 && c1 `setEq` c2
            = Just (Req a1 (b1 `pathUnion` b2) c1 d1)
        ruleSameCondAnd _ _ = Nothing
        
        ruleSubsetOr (Req a1 b1 c1 d1) (Req a2 b2 c2 d2)
            | a1 == a2 && b1 `pathSubset` b2 && c1 `setEq` c2
            = Just (Req a1 b1 c1 d1)
        ruleSubsetOr _ _ = Nothing
        
        ruleSingleOr (Req a1 b1 c1 d1) (Req a2 b2 c2 d2)
            | a1 == a2 && b1 == b2 && pathIsSingle b1
            = Just $ Req a1 b1 (c1 `union` c2) d1
        ruleSingleOr _ _ = Nothing
        
        
        ruleSingleAnd (Req a1 b1 c1 d1) (Req a2 b2 c2 d2)
            | a1 == a2 && b1 == b2 && pathIsSingle b1
            = Just $ Req a1 b1 (c1 `intersect` c2) d1
        ruleSingleAnd _ _ = Nothing
        
        
        ruleDel (PredLit (Req a1 b1 c1 d1))
            | null c1
            = Just predFalse -- not true, what if none defined - to fix
        ruleDel (PredLit (Req a1 b1 c1 d1))
            | getCtorsFromCtor hite (head c1) `setEq` c1
            = Just predTrue
        ruleDel (PredLit (Req a1 b1 c1 d1))
            | pathIsOmega b1
            = Just predTrue
        ruleDel _ = Nothing
        

        ruleImplies (Req a1 b1 c1 d1) (Req a2 b2 c2 d2)
            | a1 == a2 && (b1 `elem` diffs)
            = Just $ Req a2 b2 c2 d1
            where
                diffs = f [] (map (`rdiff` b2) iargs)
                ictors = getOtherCtors hite (head c1)
                iargs = concatMap (\x -> ctorArgs $ getCtor hite x) ictors

                rdiff x reg = pathReverse (pathQuotient x (pathReverse reg))

                f done [] = done
                f done (t:odo) | t `elem` done = f done odo
                               | otherwise = f (t:done) (t2 ++ odo)
                    where t2 = map (`rdiff` t) iargs
        ruleImplies _ _ = Nothing


        ruleImplies2 (Req a1 b1 c1 d1) (Req a2 b2 c2 d2)
            | a1 == a2 && (b1 `pathSubset` b2) && (c1 == (c1 \\ c2))
            = Just (Req a1 b1 [] d1)
        ruleImplies2 _ _ = Nothing
        -}

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


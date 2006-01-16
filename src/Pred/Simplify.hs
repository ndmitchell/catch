
module Pred.Simplify(simplifyPred) where

import Pred.Type
import Pred.Form
import List
import Maybe
import General.Simplify



simplifyPred :: (Show a, Eq a) => [Rule a] -> [Rule a] -> [Rule (Pred a)] -> Pred a -> Pred a
simplifyPred ror rand rone x = {- simp $ dnf $ -}  simp $ dnf $ simp x
    where
        simp x = simplifyPred2 ror rand rone (nubPred x)



simplifyPred2 :: Eq a => [Rule a] -> [Rule a] -> [Rule (Pred a)] -> Pred a -> Pred a
simplifyPred2 ror rand rone x = mapPred f x
    where
        f (PredAnd xs) = predAnd $ g rand xs
        f (PredOr  xs) = predOr  $ g ror  xs
        f x = x
        
        g rs xs = no ++ simplifySet (rone ++ map h rs) yes
            where (yes, no) = partition isPredLit xs
        
        h (Rule f) = Rule (prom2 f)
        h (RuleAssoc f) = RuleAssoc (prom2 f)
        h (RuleOne f) = RuleOne (prom1 f)
        
        prom1 f (PredLit a) = do {x <- f a; return $ PredLit x}
        prom1 f _ = Nothing
        
        prom2 f (PredLit a) (PredLit b) = do {x <- f a b; return $ PredLit x}
        prom2 f _ _ = Nothing
        
        fromPredLit (PredLit x) = x

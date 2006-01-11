
module Pred.Simplify(simplifyPred, PredRule(..)) where

import Pred.Type
import List
import Maybe
import General.Simplify


data PredRule a = RuleAnd (a -> a -> Maybe a)
                | RuleOr  (a -> a -> Maybe a)


isRuleAnd (RuleAnd _) = True; isRuleAnd _ = False
isRuleOr  (RuleOr  _) = True; isRuleOr  _ = False



simplifyPred :: [Rule a] -> [Rule a] -> [Rule (Pred a)] -> Pred a -> Pred a
simplifyPred ror rand rone x = mapPred f x
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

{-

simplifyPred :: [PredRule a] -> Pred a -> Pred a
simplifyPred rules x = mapPred f x
    where
        andRules = filter isRuleAnd rules
        orRules  = filter isRuleOr  rules
    
        f (PredAnd xs) = predAnd $ no ++ simpList andRules yes
            where (yes,no) = partition isPredLit xs
        
        f (PredOr  xs) = predOr  $ no ++ simpList orRules  yes
            where (yes,no) = partition isPredLit xs
            
        f x = x


simpList :: [PredRule a] -> [Pred a] -> [Pred a]
simpList rules xs = map predLit $ fixLength (applyRules rules) $ map (\(PredLit q) -> q) xs


fixLength f x = if length x == length x2 then x else fixLength f x2
    where x2 = f x


applyRules :: [PredRule a] -> [a] -> [a]
applyRules [] x = x
applyRules (r:rs) x = applyRules rs (applyRule r x)


applyRule :: PredRule a -> [a] -> [a]
applyRule (RuleAnd f) xs = applyFunc f xs
applyRule (RuleOr  f) xs = applyFunc f xs


applyFunc :: (a -> a -> Maybe a) -> [a] -> [a]
applyFunc f [] = []
applyFunc f (x:xs) =
        if all isNothing res then
            x : applyFunc f xs
        else
            g (zip res xs)
    where
        res = map (f x) xs
        
        g ((Nothing,y):ys) = y : g ys
        g ((Just a ,y):ys) = a : map snd ys
        g _ = error "applyFunc failed, logic error"

  -}      
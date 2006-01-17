
module General.Simplify(Rule(..), simplifyList, simplifySet, factor) where

import List
import Maybe


data Rule a = Rule (a -> a -> Maybe a)
            | RuleAssoc (a -> a -> Maybe a)
            | RuleOne (a -> Maybe a)
{-            | RuleDel (a -> Bool)
too dangerous for the moment, removing might not mean [], it might be short circuiting
-}




simplifyList :: [Rule a] -> [a] -> [a]
simplifyList rules xs = f vals
    where
        (vals, r) = prepare rules xs
        
        f (x:y:zs) = case r x y of
                        Just a  -> f (a ++ zs)
                        Nothing -> x : f (y:zs)
        f x = x
    


simplifySet :: [Rule a] -> [a] -> [a]
simplifySet rules xs = f [] vals
    where
        (vals, r) = prepare rules xs
        
        f done [] = reverse done
        f done (x:xs) =
            if all isNothing res then
                f (x:done) xs
            else
                f [] $ reverse done ++ g (zip res xs)
            where
                res = map (r x) xs

        g ((Nothing,y):ys) = y : g ys
        g ((Just a ,y):ys) = a ++ map snd ys
        g _ = error "applyFunc failed, logic error"



prepare :: [Rule a] -> [a] -> ([a], a -> a -> Maybe [a])
prepare rules xs = (concatMap allOne xs, allBoth)
    where
        rul = concatMap remAssoc rules
        one = [x | RuleOne x <- rul]
        many = [x | Rule x <- rul]
        del = [] -- [x | RuleDel x <- rul]
    
        allOne = collapseDel del . collapseOne one
        allMany = collapseMany many
        
        allBoth a b = do x <- allMany a b
                         return $ allOne x
                         
        collapseDel xs a = if or (map (\f -> f a) xs) then [] else [a]
        
        collapseOne orig a = f orig a
            where
                f [] a = a
                f (x:xs) a = case x a of
                                Nothing -> f xs a
                                Just a  -> f orig a 

        collapseMany [] a b = Nothing
        collapseMany (x:xs) a b = case x a b of
                                      Nothing -> collapseMany xs a b
                                      Just a -> Just a
        
        remAssoc (RuleAssoc f) = [Rule f, Rule (flip f)]
        remAssoc x = [x]



factor :: Eq a => [a] -> [a] -> Maybe ([a], ([a], [a]), [a])
factor as bs = if null pre && null post then Nothing
               else Just (pre, (reverse a3, reverse b3), reverse post)
    where
        (pre, (a2, b2)) = facSide as bs
        (post, (a3, b3)) = facSide (reverse a2) (reverse b2)


        -- :: [a] -> [a] -> ([a], ([a], [a]))
        facSide (a:as) (b:bs) | a == b = (a:pre, (a2, b2))
            where (pre, (a2, b2)) = facSide as bs
        facSide as bs = ([], (as, bs))

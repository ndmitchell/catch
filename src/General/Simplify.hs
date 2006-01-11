
module General.Simplify(Rule(..), simplifyList, simplifySet) where

import List
import Maybe


data Rule a = Rule (a -> a -> Maybe a)
            | RuleAssoc (a -> a -> Maybe a)
            | RuleOne (a -> Maybe a)




simplifyList :: [Rule a] -> [a] -> [a]
simplifyList rules xs = f vals
    where
        (vals, r) = prepare rules xs
        
        f (x:y:zs) = case r x y of
                        Just a  -> f (a:zs)
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
        g ((Just a ,y):ys) = a : map snd ys
        g _ = error "applyFunc failed, logic error"



prepare :: [Rule a] -> [a] -> ([a], a -> a -> Maybe a)
prepare rules xs = (map allOne xs, allBoth)
    where
        (one, many) = partition isOne (concatMap remAssoc rules)
        allOne = collapseOne one
        allMany = collapseMany many
        
        allBoth a b = do x <- allMany a b
                         return $ allOne x
        
        collapseOne orig a = f orig a
            where
                f [] a = a
                f (RuleOne x:xs) a = case x a of
                                Nothing -> f xs a
                                Just a  -> f orig a 

        collapseMany [] a b = Nothing
        collapseMany (Rule x:xs) a b = case x a b of
                                      Nothing -> collapseMany xs a b
                                      Just a -> Just a
        
        remAssoc (RuleAssoc f) = [Rule f, Rule (flip f)]
        remAssoc x = [x]
        
        isOne (RuleOne _) = True
        isOne _ = False
        


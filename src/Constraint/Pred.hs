module Constraint.Pred where

import List


data Pred a = Ors  [Pred a]
            | Ands [Pred a]
            | PredTrue
            | PredFalse
            | PredAtom a
            deriving (Eq)



mapPred :: (Pred a -> Pred a) -> Pred a -> Pred a
mapPred f x = f $ case x of
        Ors  xs -> Ors  (fs xs)
        Ands xs -> Ands (fs xs)
        x -> x
    where
        fs = map (mapPred f)


mapPredM :: Monad m => (Pred a -> m (Pred a)) -> Pred a -> m (Pred a)
mapPredM f x = do y <- case x of
                      Ors xs  -> do ys <- fs xs
                                    return $ Ors xs
                      Ands xs -> do ys <- fs xs
                                    return $ Ands xs
                      x -> return x
                  f y
    where
        fs xs = mapM (mapPredM f) xs


allPred :: Pred a -> [Pred a]
allPred x = x : concatMap allPred (case x of
        Ands y -> y
        Ors  y -> y
        _ -> []
        )


reducePred :: Pred a -> Pred a
reducePred x = mapPred f x
    where
        f (Ors  [x]) = x
        f (Ands [x]) = x
        
        f (Ors  xs) = case concatMap g_or  xs of
                        [x] -> x
                        xs -> Ors xs
                        
        f (Ands xs) = case concatMap g_and xs of
                        [x] -> x
                        xs -> Ands xs
        
        f x = x
        
        g_or (Ors x) = x
        g_or x = [x]
        
        g_and (Ands x) = x
        g_and x = [x]
        
        {-
        f (Ors xs) = f $ Ors (filter isFalse xs)
        f (Ands xs) = Ands (filter isTrue xs)
        f x = x
        -}



isFalse (Ors  []) = True
isFalse (Ands xs) = any isFalse xs
isFalse (Ors  xs) = all isFalse xs
isFalse _ = False

isTrue (Ands []) = True
isTrue (Ands xs) = all isTrue xs
isTrue (Ors  xs) = any isTrue xs
isTrue _ = False

{-

type Path = Regex Sel

-- SHOW INSTANCES
instance Show Req where
    show (Req func n path ctor) =
            func ++ "#" ++ show n ++ "." ++ show path ++ ":" ++ f ctor
        where
            f [x] = x
            f xs  = "(" ++ concat (intersperse "," xs) ++ ")"
-}

instance Show a => Show (Pred a) where
    show x = case x of
            PredTrue -> "True"
            PredFalse -> "False"
            PredAtom a -> show a
            Ors  x -> disp 'v' x
            Ands x -> disp '^' x
        where
            disp sym xs = "[" ++ x3 ++ "]"
                where
                    x3 = concat $ intersperse [' ',sym,' '] x2
                    x2 = if any f xs then map (\x -> "(" ++ x ++ ")") x1 else x1
                    x1 = map show xs

                    f (Ors  _) = True
                    f (Ands _) = True
                    f _ = False



{-

-- required 'is' functions
isPredTrue PredTrue = True; isPredTrue _ = False
isPredFalse (PredFalse _) = True; isPredFalse _ = False



isPathEwp :: Path -> Bool
isPathEwp x = ewp x


-- invert a requirement
notReq :: Hite -> Req -> Req
notReq hite req = req{reqCtors = allCtors hite (head ctors) \\ ctors}
    where ctors = reqCtors req


-- move down a path
pathMove :: Path -> Sel -> Maybe Path
pathMove path sel = if empty path2 then Nothing else Just path2
    where path2 = simp $ diff path sel

pathJoinUnion pre xs = simp $ rcon [runi (map RLit pre)] xs

pathJoin pre xs = simp $ rcon (map RLit pre) xs

asPath [ ] = lambda
asPath [x] = RLit x
asPath xs  = RCon $ map RLit xs



mapReq :: (Req -> Pred) -> Pred -> Pred
mapReq f (Ors     x) = Ors  (map (mapReq f) x)
mapReq f (Ands    x) = Ands (map (mapReq f) x)
mapReq f (PredReq x) = f x
mapReq f x           = x


anyReq :: Pred -> Bool
anyReq (Ors     x) = any anyReq x
anyReq (Ands    x) = any anyReq x
anyReq (PredReq _) = True
anyReq _ = False


allReq :: Pred -> [Req]
allReq (Ors     x) = concat (map allReq x)
allReq (Ands    x) = concat (map allReq x)
allReq (PredReq x) = [x]
allReq _           = []


mapReqList :: (Req -> ([a], Pred)) -> Pred -> ([a], Pred)
mapReqList f predicate = 
    case predicate of
        Ors     x -> g Ors  x
        Ands    x -> g Ands x
        PredReq x -> f x
        x         -> ([], x)
    where
        g con xs = (concat (fsts res), con (snds res))
            where res = map (mapReqList f) xs



mapReqState :: (a -> Req -> (a, Pred)) -> a -> Pred -> (a, Pred)
mapReqState f state predicate =
    case predicate of
        Ors     x -> g Ors  state [] x
        Ands    x -> g Ands state [] x
        PredReq x -> f state x
        x         -> (state, x)
    where
        g ctor state done [] = (state, ctor (reverse done))
        g ctor state done (x:xs) = g ctor s2 (p2:done) xs
            where (s2, p2) = mapReqState f state x
    


-- predicate logic simplifier
-- must preserve false statements!
-- RULES

-- flatten
-- And [And x, y] -> And [x, y]
-- Or  [Or  x, y] -> Or  [x, y]

-- reduce
-- And [x] -> x
-- Or  [x] -> x

-- logic
-- Or  [True, x] -> True
-- And [True, x] -> And [x]

-- only instance of True allowed is as the root
simpPred :: Pred -> Pred

simpPred (Ands xs) = 
        case nub $ filter (not . isPredTrue) $ concatMap (f . simpPred) xs of
            []  -> PredTrue
            x   -> makeOne Ands x
    where
        f (Ands x) = x
        f x        = [x]

simpPred (Ors xs) = 
        case nub $ concatMap (f . simpPred) xs of
            [x] -> x
            x   -> if any isPredTrue x then PredTrue else
                   if all isPredFalse x then PredFalse (g x) else
                   makeOne Ors (filter (not . isPredFalse) x)
    where
        f (Ors x) = x
        f x       = [x]
        
        g xs = concat $ intersperse " v " $ map (\(PredFalse x) -> x) xs
        

simpPred x = x


makeOne f [x] = x
makeOne f xs  = f xs


predEq a b = a == b



simpReq req = PredReq req{reqPath = simp (reqPath req), reqCtors = sort (reqCtors req)}


simpPredReq x = mapPred simpReqPairs (mapReq simpReq (simpPred x))

simpReqPairs (Ands xs) = Ands (simpReqPairsF simpPredReqAnd xs)
simpReqPairs (Ors  xs) = Ors  (simpReqPairsF simpPredReqOr  xs)

simpReqPairsF f xs = case xs of
        (PredReq x:xs) -> g x xs
        (x:xs) -> x : simpReqPairsF f xs
        [] -> []
    where
        g a (x@(PredReq b):xs) = 
            case f a b of
                Just x2 -> g x2 xs
                Nothing -> x : g a xs
        g a (x:xs) = x : g a xs
        g a [] = [PredReq a]


simpPredReqAnd x@(Req n1 v1 p1 c1) (Req n2 v2 p2 c2)
    | n1 == n2 && v1 == v2 && p1 == p2 = Just x{reqCtors = intersect c1 c2}
    | n1 == n2 && v1 == v2 && c1 == c2 = Just x{reqPath = runi [p1,p2]}
    | otherwise = Nothing

simpPredReqOr a@(Req n1 v1 p1 c1) b@(Req n2 v2 p2 c2)
    | n1 == n2 && v1 == v2 && redundant a b = Just b
    | n1 == n2 && v1 == v2 && redundant b a = Just a
    | otherwise = Nothing


-- is the first redundant, given the second 
redundant (Req _ _ p1 c1) (Req _ _ p2 c2) =
        length pre2 >= length pre1 && and (zipWith (==) pre1 pre2) && not (ewp end) &&
        (nub (map selName (prefixes end)) `intersect` c1) == []
    where
        pre1 = asRCon p1
        pre2 = asRCon p2
        end = RCon $ drop (length pre1) pre2

prefixes (RCon [a]) = prefixes a
prefixes (RCon (a:as)) | ewp a = prefixes a ++ prefixes (RCon as)
                       | otherwise = prefixes a
prefixes (RUni as) = concatMap prefixes as
prefixes (RStar as) = prefixes as
prefixes (RLit a) = [a]
        


asRCon (RCon x) = x
asRCon x = [x]

-}

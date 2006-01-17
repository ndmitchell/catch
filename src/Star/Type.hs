
module Star.Type where

import List
import General.General
import General.Simplify

import Debug.Trace

{-
RULES:
A well constructed Star will have the following properties

Lambda or Omega can only ever be the VERY TOP element, not in the children
Star must have a sorted list of items
Star _ [0] False is Lambda, and therefore cannot occur

if two elements are starEq, i.e. they are equal apart from the star surrounding them
then they may not appear in the same uni, int or next to each other in a con
star star is allowed sometimes (aa)* for example
-}

data (Show a, Eq a) =>
    Star a = Star (Star a) [Int] Bool
           | Lambda
           | Omega
           | StarUni [Star a]
           | StarInt [Star a]
           | StarCon [Star a]
           | StarLit a
           deriving Show



mapStar :: (Show a, Eq a) => (Star a -> Star a) -> Star a -> Star a
mapStar f x = f $ case x of
        Star x z b -> star (g x) z b
        StarUni x -> starUni (gs x)
        StarCon x -> starCon (gs x)
        StarInt x -> starInt (gs x)
        x -> x
    where
        g x = mapStar f x
        gs = map g
        

-- is #L(r) = 1
-- conservative
isSingle :: (Show a, Eq a) => Star a -> Bool
isSingle (StarLit _) = True
isSingle Lambda      = True
isSingle Omega       = False
isSingle (StarInt x) = any isSingle x
isSingle (StarUni x) = all isSingle x
isSingle (StarCon x) = False
isSingle (Star z b x) = False


-- is \forall l \in L(r), h `elem` l
-- conservative - output true if in doubt
hasLetter c Lambda = False
hasLetter c Omega  = False
hasLetter c (StarLit x) = c == x
hasLetter c (StarInt x) = all (hasLetter c) x
hasLetter c (StarUni x) = any (hasLetter c) x
hasLetter c (StarCon x) = any (hasLetter c) x
hasLetter c (Star x z b) | 0 `elem` z = False
                         | otherwise  = hasLetter c x


allStarLit (StarLit x) = [x]
allStarLit (StarUni x) = concatMap allStarLit x
allStarLit (StarInt x) = concatMap allStarLit x
allStarLit (StarCon x) = concatMap allStarLit x
allStarLit (Star x z b) = allStarLit x
allStarLit _ = []


-- is Lambda `elem` L(r)
isEwp :: (Show a, Eq a) => Star a -> Bool
isEwp (Star x z b) = (0 `elem` z) || isEwp x
isEwp Lambda = True
isEwp Omega  = False
isEwp (StarCon x) = all isEwp x
isEwp (StarInt x) = all isEwp x
isEwp (StarUni x) = any isEwp x
isEwp (StarLit x) = False



integrate :: (Show a, Eq a) => a -> Star a -> Star a
integrate x y = starCon [starLit x, y]


quotient :: (Show a, Eq a) => a -> Star a -> Star a
quotient y Lambda = Omega
quotient y Omega  = Omega
quotient y s@(Star x z b) = quotient y $ unroll s

quotient y (StarLit x) = if x == y then Lambda else Omega

quotient y (StarUni x) = starUni (map (quotient y) x)
quotient y (StarInt x) = starInt (map (quotient y) x)

quotient y (StarCon (x:xs))
    | isEwp x = starUni [starCon (quotient y x:xs), quotient y (starCon xs)]
    | otherwise = starCon (quotient y x:xs)


-- Eq definition
instance (Eq a, Show a) => Eq (Star a) where
    (Star a b c) == (Star d e f) = a == d && b == e && c == f
    Lambda == Lambda = True
    Omega == Omega = True
    (StarCon x) == (StarCon y) = x == y
    (StarInt x) == (StarInt y) = x `setEq` y
    (StarUni x) == (StarUni y) = x `setEq` y
    (StarLit x) == (StarLit y) = x == y
    a == b = False


-- useful internal methods

unroll (Star x z b) = StarCon [x, star x (map ((-1)+) z) b]

wrapStar s@(Star {}) = s
wrapStar x = Star x [1] False

unwrapStar (Star x [1] False) = x
unwrapStar x = x

makeOne f [] = error "makeOne, []"
makeOne f [x] = x
makeOne f xs  = f xs

(Star a _ _) =*= (Star b _ _) = a == b


fromStarCon (StarCon xs) = xs
fromStarCon x = [x]


multisetBy :: (a -> a -> Bool) -> [a] -> [[a]]
multisetBy f [] = []
multisetBy f (x:xs) = (x:yes) : multisetBy f no
    where (yes, no) = partition (f x) xs


-- and the biggest internal one
-- rules, and what it does
--    remove all that are less than 0
--    sort, nub
--    if inf is set, then (n,n+1) -> n at the top end
--    if [1] False, then just return item
-- note: doesn't currently lift if x is a star
star :: (Show a, Eq a) => Star a -> [Int] -> Bool -> Star a
star x z b = if res2 == [1] && (not b) then x
             else if null res2 then (if b then Lambda else Omega)
             else Star x res2 b
    where
        res2 = if b then reverse $ dropNext $ reverse res else res
        res = map head $ group $ sort $ filter (>= 0) z
        
        dropNext (a:b:c) | a - b == 1 = dropNext (b:c)
        dropNext x = x


starLit :: (Show a, Eq a) => a -> Star a
starLit x = StarLit x


starUni :: (Show a, Eq a) => [Star a] -> Star a
starUni xs = if null res then Omega
             else if null res2 then Lambda
             else makeOne StarUni $ starFact starUni res3
    where
        res3 = map (unwrapStar . join) $ multisetBy (=*=) res2
        res2 = map (if Lambda `elem` res then joinLambda else id) $
               [wrapStar x | x <- concatMap explode xs, x /= Lambda]
        res  = filter (/=Omega) $ concatMap explode xs
    
        join x = foldr1 joinPair x
        joinPair (Star x z1 b1) (Star _ z2 b2) = wrapStar $ star x (z1 `union` z2) (b1 || b2)
        
        joinLambda (Star x z b) = star x (0:z) b
    
        explode (StarUni xs) = xs
        explode x = [x]


starInt :: (Show a, Eq a) => [Star a] -> Star a
starInt xs = if Omega `elem` res then Omega
             else if Lambda `elem` res then Lambda
             else if disjoint res2 then {- trace ("{{{" ++ show res2 ++ "}}}")-} Omega
             else makeOne StarInt $ starFact starInt res2
    where
        res2 = map (unwrapStar . join) $ multisetBy (=*=) $
               map wrapStar res
        res = concatMap explode xs

        join x = foldr1 joinPair x
        joinPair (Star x z1 b1) (Star _ z2 b2) = wrapStar $ star x (z1 `intersect` z2) (b1 && b2)
        
        explode (StarInt xs) = xs
        explode x = [x]
        
        disjoint xs = or [f a b | a <- xs, b <- xs]

        f a b = any (`hasLetter` a) $ nub (allStarLit a) \\ nub (allStarLit b)
        
        


starCon :: (Show a, Eq a) => [Star a] -> Star a
starCon xs = if Omega `elem` res then Omega
             else if null res then Lambda
             else makeOne StarCon res2
    where
        res2 = map unwrapStar $ join $ map wrapStar res
        res = filter (/= Lambda) $ concatMap explode xs
        
        join (Star x1 z1 b1 : Star x2 z2 b2 : xs)
            | x1 == x2 = join (wrapStar (star x1 (z1 `plus` z2) (b1 || b2)) : xs)
        join (x:xs) = x : join xs
        join [] = []
        
        plus as bs = [a+b | a <- as, b <- bs]
        
        explode (StarCon xs) = xs
        explode x = [x]


starRev x = mapStar f x
    where
        f (StarCon xs) = StarCon $ reverse xs
        f x = x

starFact :: (Show a, Eq a) => ([Star a] -> Star a) -> [Star a] -> [Star a]
starFact join xs = simplifySet [Rule f] xs
    where
        f a b = do (pre, (as, bs), post) <- factor (fromStarCon a) (fromStarCon b)
                   return $ starCon $ pre ++ [join [starCon as, starCon bs]] ++ post

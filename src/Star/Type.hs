
module Star.Type(
    integrate,
    quotient,
    isEwp
    ) where

import List
import General.General

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


-- useful internal methods

unroll (Star x z b) = star x (map ((-1)+) z) b

wrapStar s@(Star {}) = s
wrapStar x = Star x [1] False

unwrapStar (Star x [1] False) = x
unwrapStar x = x

makeOne f [] = error "makeOne, []"
makeOne f [x] = x
makeOne f xs  = f xs

(Star a _ _) =*= (Star b _ _) = a == b


multisetBy :: (a -> a -> Bool) -> [a] -> [[a]]
multisetBy f [] = []
multisetBy f (x:xs) = (x:yes) : multisetBy f no
    where (yes, no) = partition (f x) xs


-- and the biggest internal one
star :: (Show a, Eq a) => Star a -> [Int] -> Bool -> Star a
star = error "todo"


starLit :: (Show a, Eq a) => a -> Star a
starLit x = StarLit x


starUni :: (Show a, Eq a) => [Star a] -> Star a
starUni xs = if null res then Omega
             else if null res2 then Lambda
             else makeOne StarUni res3
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
             else makeOne StarInt res2
    where
        res2 = map (unwrapStar . join) $ multisetBy (=*=) $
               map wrapStar res
        res = concatMap explode xs

        join x = foldr1 joinPair x
        joinPair (Star x z1 b1) (Star _ z2 b2) = wrapStar $ star x (z1 `intersect` z2) (b1 && b2)
        
        explode (StarInt xs) = xs
        explode x = [x]


starCon :: (Show a, Eq a) => [Star a] -> Star a
starCon xs = if Omega `elem` res then Omega
             else if null res then Lambda
             else makeOne StarCon res2
    where
        res2 = map unwrapStar (join res)
        res = filter (/= Lambda) $ concatMap explode xs
        
        join (Star x1 z1 b1 : Star x2 z2 b2 : xs)
            | x1 == x2 = join (wrapStar (star x1 (z1 ++ z2) (b1 || b2)) : xs)
        join (x:xs) = x : join xs
        
        explode (StarCon xs) = xs
        explode x = [x]

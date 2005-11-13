-- taken mainly from Helix, which was taken partly from Rex

module Constraint.RegExp where

data RegExp a = RLit a
              | ROmega
              | RStar (RegExp a)
              | RCon  [RegExp a]
              | RUni  [RegExp a]
              deriving (Eq) --ouch, not truely equality, just guess work


{-
STOLEN SHAMELESSLY FROM 3rd YEAR PROJECT

display a regular expression as a piece of text, done this way to make it useful
in both the program and during development

Aim is to show it with the minimal amount of brackets
Kleene Omega is automatically transformed into "^", representing Lamdba
-}

{-
implement an ordered Precedence relationship
a < b means that if a regular expression of type a is combined under b it must be bracketed
-}
data Order =  OUnion | OConcat | OKleene
    deriving (Eq, Ord)


instance Show a => Show (RegExp a) where
    show x = f OUnion x
        where
            f :: Show a => Order -> RegExp a -> String

            f p (RLit x) = show x

            f p (RStar ROmega) = "^"
            f p (ROmega) = "0"

            -- show kleene star with prefix notation, as thats better for this purpose
            f p (RStar x) = '*' : f OKleene x
            f p (RCon xs) = brack (OConcat < p) (tail (foldr join "" xs))
                where join x xs  = '.' : f OConcat x ++ xs

            f p (RUni x) = brack (OUnion < p) (tail (foldr join "" x))
                where join x xs = '+' : f OUnion x ++ xs

            brack False s = s
            brack True  s = '(' : s ++ ")"
-- END STOLEN


runi :: (Eq a, Show a) => [RegExp a] -> RegExp a
runi [RCon a,RCon b] | a == b = RCon a
                     | length a == length b = res
    where
        res = RCon (take pre a ++ [mids] ++ drop (pre+mid) a)
        
        matches = zipWith (==) a b
        pre = length $ takeWhile id matches
        post = length $ takeWhile id (reverse matches)
        mid = length a - pre - post
        mids = RUni [RCon $ take mid (drop pre a), RCon $ take mid (drop pre b)]
        

runi xs = RUni xs

rcon :: [RegExp a] -> RegExp a -> RegExp a
rcon [] b = b
rcon a (RCon x) = RCon (a++x)
rcon a b = RCon (a++[b])

rcon2 :: RegExp a -> [RegExp a] -> RegExp a
rcon2 a [] = a
rcon2 (RCon x) b = RCon (x++b)
rcon2 a b = RCon (a:b)


lambda = RStar ROmega


simp :: Eq a => RegExp a -> RegExp a
simp (RCon xs ) = if ROmega `elem` res then ROmega else makeList RCon lambda res
    where
        res = (concatMap (f . simp) xs)
        
        f (RCon x) = x
        f (RStar (ROmega)) = []
        f x          = [x]

simp (RUni   xs) = makeList RUni ROmega (concatMap (f . simp) xs)
    where f (RUni x) = x
          f (ROmega) = []
          f x        = [x]

simp (RStar  x ) = RStar (simp x)
simp x            = x


makeList f g []  = g
makeList f g [x] = x
makeList f g xs  = f xs




ewp :: RegExp a -> Bool
ewp (RCon xs) = all ewp xs
ewp (RUni xs) = any ewp xs
ewp (RStar x) = True
ewp (RLit  x) = False
ewp (ROmega ) = False


-- emptiness property (no strings in the language)
-- only works if simp is called first
empty :: RegExp a -> Bool
empty (RUni a) = all empty a
empty (RCon a) = any empty a
empty (ROmega) = True
empty _        = False


diff :: Eq a => RegExp a -> a -> RegExp a
diff (RLit x) y = if x == y then RStar ROmega else ROmega
diff (RCon [x]) y = diff x y
diff (RCon (x:xs)) y | ewp x = RUni [RCon (diff x y:xs), diff (RCon xs) y]
                     | otherwise = RCon (diff x y:xs)
diff (RUni xs) y = RUni (map (`diff` y) xs)
diff (RStar x) y = RCon [diff x y, RStar x]

diff (ROmega ) y = ROmega

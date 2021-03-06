
module General(module General, trace) where

import Maybe
import Directory
import List
import Monad
import Data.Char
import Foreign(unsafePerformIO)
import Debug.Trace

---------------------------------------------------------------------
-- SIMPLE UTILITIES

errorS a = error (show a)
traceS a b = trace (show a) b

box = (:[])

fsts = map fst
snds = map snd

snub :: Ord a => [a] -> [a]
snub = map head . group . sort

filterFst f = filter (f . fst)
filterSnd f = filter (f . snd)

splitEither :: [Either a b] -> ([a], [b])
splitEither (Left  x:xs) = let (a,b) = splitEither xs in (x:a, b)
splitEither (Right x:xs) = let (a,b) = splitEither xs in (a, x:b)
splitEither [] = ([], [])

interleave = (/\/)

(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)



numToChr :: Int -> Char
numToChr i = chr (ord 'a' + i)

-- requires both arguments are the same length, or crashes
zipWithEq :: (Show a, Show b) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWithEq f xs ys = g xs ys
    where
        g (x:xs) (y:ys) = f x y : g xs ys
        g [] [] = []
        g _ _ = error $ "zipWithEq, different lengths, " ++ show (xs,ys)

zipWithRest :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithRest f (x:xs) (y:ys) = f x y : zipWithRest f xs ys
zipWithRest f xs ys = xs ++ ys

singleton [x] = True
singleton _ = False


fromRight (Right x) = x
fromLeft  (Left  x) = x

---------------------------------------------------------------------
-- SAFE VERSIONS


zipWithEqNote :: (Show a, Show b) => String -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithEqNote msg f x y = g x y
    where
        g [] [] = []
        g (x:xs) (y:ys) = f x y : g xs ys
        g _ _ = error $ "zipWithEqNote " ++ msg ++ ", on " ++ show (x,y)



---------------------------------------------------------------------
-- LIST AS SET STUFF

setEq :: Eq a => [a] -> [a] -> Bool
setEq = setEqBy (==)


-- | Are two sets eq, given the appropriate equality test.
setEqBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
setEqBy f (x:xs) ys = case remElem f x ys of
                          Just a -> setEqBy f xs a
                          Nothing -> False
setEqBy f [] [] = True
setEqBy _ _ _ = False


-- | Remove an element from a set, using the given equality test.
--   Returns Nothing if the element was not in the set
remElem :: (a -> a -> Bool) -> a -> [a] -> Maybe [a]
remElem f a xs = g [] xs
    where
        g _ [] = Nothing
        g done (x:xs) | f x a = Just (done++xs)
                      | otherwise = g (x:done) xs


disjoint :: Eq a => [a] -> [a] -> Bool
disjoint x = null . intersect x


overlap :: Eq a => [a] -> [a] -> Bool
overlap x = not . disjoint x


eqUnordered :: Ord a => [a] -> [a] -> Bool
eqUnordered xs ys = sort xs == sort ys


allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs

groupSetBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupSetBy f [] = []
groupSetBy f (x:xs) = (x:match) : groupSetBy f rest
    where (match,rest) = partition (f x) xs


subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs


---------------------------------------------------------------------
-- EXTRACT VERSIONS

nubExtract :: Eq b => (a -> b) -> [a] -> [a]
nubExtract f xs = nubBy cmp xs
    where cmp a b = f a == f b

sortExtract :: Ord b => (a -> b) -> [a] -> [a]
sortExtract f xs = map snd $ sortBy cmp [(f x, x) | x <- xs]
    where cmp a b = fst a `compare` fst b

minimumExtract :: Ord b => (a -> b) -> [a] -> a
minimumExtract f xs = head $ sortExtract f xs

groupSetExtract :: Eq b => (a -> b) -> [a] -> [[a]]
groupSetExtract f x = groupSetBy cmp x
    where cmp a b = f a == f b


---------------------------------------------------------------------
-- COMBINATORIAL STUFF

allItems :: [a] -> [([a], a, [a])]
allItems [] = []
allItems (x:xs) = ([], x, xs) : [(x:a,b,c) | (a,b,c) <- allItems xs]


crossProduct :: [[a]] -> [[a]]
crossProduct (x:xs) = [y:ys | y <- x, ys <- crossProduct xs]
crossProduct [] = [[]]



powerSet       :: [a] -> [[a]]
powerSet []     = [[]]
powerSet (x:xs) = xss /\/ map (x:) xss
                 where xss = powerSet xs


allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = map ((,) x) xs ++ allPairs xs

---------------------------------------------------------------------
-- OUTPUT STUFF


strSet :: [String] -> String
strSet xs = "{" ++ intercat "," xs ++ "}"


indent x = "  " ++ x
indents x = map indent x
indentStr = unlines . map ("    " ++) . lines



intercat :: String -> [String] -> String
intercat sep = concat . intersperse sep

intercatS :: Show a => String -> [a] -> String
intercatS sep = intercat sep . map show

showLines :: Show a => [a] -> String
showLines = unlines . map show



---------------------------------------------------------------------
-- FIXPOINT STUFF



fixBound :: Eq a => Int -> (a -> a) -> a -> a
fixBound 0 f x = x
fixBound n f x = if x == x2 then x else fixBound (n-1) f x2
	where x2 = f x


fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x2 then x else fix f x2
    where x2 = f x


-- find the fixed point of a set
fixSet :: Eq a => (a -> [a]) -> [a] -> [a]
fixSet f elems = fix2 f elems []
    where
        fix2 f [] _    = []
        fix2 f x  done = x ++ fix2 f (x2 \\ done2) done2
            where
                done2 = x ++ done
                x2 = nub $ concatMap f x


---------------------------------------------------------------------
-- MANIPULATE STUFF

class Manipulate a where
    replaceChildren :: a -> ([a], [a] -> a)

    getChildren :: a -> [a]
    getChildren = fst . replaceChildren

    setChildren :: a -> [a] -> a
    setChildren = snd . replaceChildren


allOverOld :: Manipulate a => a -> [a]
allOverOld x = x : concatMap allOverOld (getChildren x)


mapOverOld :: Manipulate a => (a -> a) -> a -> a
mapOverOld f x = f $ setChildren x $ map (mapOverOld f) $ getChildren x


mapOverOldM :: (Monad m, Manipulate a) => (a -> m a) -> a -> m a
mapOverOldM f x = f . setChildren x =<< mapM (mapOverOldM f) (getChildren x)


---------------------------------------------------------------------
-- IO STUFF



ensureDirectory s = do b <- doesDirectoryExist s
                       when (not b) $ createDirectory s



readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe file = do x <- doesFileExist file
                        if x
                            then do y <- readFile file
                                    return (Just y)
                            else return Nothing


---------------------------------------------------------------------
-- DEBUG STUFF



traceFunc :: Show a => String -> a -> a
traceFunc args res = trace (args ++ " => " ++ show res) res

{-unsafePerformIO $ do
    let str = reverse $ reverse $ args ++ " = " ++ show res
    appendFile "trace.log" (str ++ "\n")
    return res
-}

traceNone :: Show a => String -> a -> a
traceNone args res = res



---------------------------------------------------------------------
-- MISC STUFF



lexes :: String -> [String]
lexes x = case lex x of
              [("","")] -> []
              [(y,ys)] -> y : lexes ys



-- replace a given element
(!!!) :: [a] -> (Int, a) -> [a]
[] !!! _ = error "!!!"
(x:xs) !!! (0,y) = y:xs
(x:xs) !!! (n,y) = x : (xs !!! (n-1,y))


-- delete a given element
(\!!) :: [a] -> Int -> [a]
(x:xs) \!! 0 = xs
(x:xs) \!! n = x : (xs \!! (n-1))




class Blur x where
    blur :: x -> x




mapId :: (a -> Int -> (Int, b)) -> [a] -> Int -> (Int, [b])
mapId f [] n = (n,[])
mapId f (x:xs) n = (n3, x2:x3)
    where
        (n2,x2) = f x n
        (n3,x3) = mapId f xs n2

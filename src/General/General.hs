
module General.General where

import Maybe
import Directory
import List
import Monad
import Data.Predicate

{-
import IOExts

debugmsg msg val = unsafePerformIO $
                        do putStrLn (show msg)
                           return val

debugmsgres msg val = debugmsg (msg, val) val
-}

errorS a = error (show a)

fsts = map fst
snds = map snd

filterFst f = filter (f . fst)
filterSnd f = filter (f . snd)

lookupDef def val lst = case lookup val lst of
                            Nothing -> def
                            Just x -> x


readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe file = do x <- doesFileExist file
                        if x
                            then do y <- readFile file
                                    return (Just y)
                            else return Nothing


eqUnordered :: Ord a => [a] -> [a] -> Bool
eqUnordered xs ys = sort xs == sort ys


strSet :: [String] -> String
strSet xs = "{" ++ concat (intersperse "," xs) ++ "}"


splitEither :: [Either a b] -> ([a], [b])
splitEither (Left  x:xs) = let (a,b) = splitEither xs in (x:a, b)
splitEither (Right x:xs) = let (a,b) = splitEither xs in (a, x:b)
splitEither [] = ([], [])


headNote msg (x:xs) = x
headNote msg [] = error $ "headNote: [] (" ++ msg ++ ")"

fromJustNote msg (Just a) = a
fromJustNote msg Nothing = error $ "fromJust2: Nothing (" ++ msg ++ ")"

lookupJust x ys = case lookup x ys of
                       Nothing -> error $ "lookupJust, " ++ show x
                       Just y -> y

lookupNote msg x ys = case lookup x ys of
                           Nothing -> error $ "lookupNote, " ++ show x ++ ", " ++ msg
                           Just y -> y

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


setEq :: Eq a => [a] -> [a] -> Bool
setEq = setEqBy (==)


groupSetBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupSetBy f [] = []
groupSetBy f (x:xs) = (x:match) : groupSetBy f rest
    where (match,rest) = partition (f x) xs


indent x = "  " ++ x
indents x = map indent x


fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x2 then x else fix f x2
    where x2 = f x


(!!!) :: [a] -> (Int, a) -> [a]
[] !!! _ = error "!!!"
(x:xs) !!! (0,y) = y:xs
(x:xs) !!! (n,y) = x : (xs !!! (n-1,y))



class Output x where
    output :: x -> String

instance Output a => Output (Pred a) where
    output x = showPredBy output x


class Blur x where
    blur :: x -> x


ensureDirectory s = do b <- doesDirectoryExist s
                       when (not b) $ createDirectory s


crossProduct :: [[a]] -> [[a]]
crossProduct (x:xs) = [y:ys | y <- x, ys <- crossProduct xs]
crossProduct [] = [[]]


-- requires both arguments are the same length, or crashes
zipWithEq :: (Show a, Show b) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWithEq f xs ys = g xs ys
    where
        g (x:xs) (y:ys) = f x y : g xs ys
        g [] [] = []
        g _ _ = error $ "zipWithEq, different lengths, " ++ show (xs,ys)

zipWithEqNote :: (Show a, Show b) => String -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithEqNote msg f x y = g x y
    where
        g [] [] = []
        g (x:xs) (y:ys) = f x y : g xs ys
        g _ _ = error $ "zipWithEqNote " ++ msg ++ ", on " ++ show (x,y)

zipWithRest :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithRest f (x:xs) (y:ys) = f x y : zipWithRest f xs ys
zipWithRest f xs ys = xs ++ ys


showLines :: Show a => [a] -> String
showLines = unlines . map show

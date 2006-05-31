
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

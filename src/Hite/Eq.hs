

module Hite.Eq() where

import Hite.Type
import List


compareUnordered :: (Eq a, Ord b) => (a -> b) -> [a] -> [a] -> Bool
compareUnordered f xs ys =
        (length xs == length ys) &&
        and (zipWith (==) (sortBy g xs) (sortBy g ys))
    where
        g a b = f a `compare` f b


instance Eq Hite where
    (Hite da fa) == (Hite db fb) = 
        compareUnordered dataName da db &&
        compareUnordered funcName fa fb

instance Eq Data where
    (Data na ca _) == (Data nb cb _) =
        na == nb &&
        compareUnordered ctorName ca cb


module General.General(module General.General, trace) where

import Data.List
import Debug.Trace


type Result a = (Bool,a)

success = (,) True
failure = (,) False


snub :: Ord a => [a] -> [a]
snub = map head . group . sort 


traceStrict msg res | length msg >= 0 = trace msg res


groupKey :: Ord key => [(key,val)] -> [(key,[val])]
groupKey = map f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    where f xs = (fst $ head xs, map snd xs)


on :: (new -> new -> res) -> (orig -> new) -> (orig -> orig -> res)
on f g x y = f (g x) (g y)

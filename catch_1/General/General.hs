
module General.General(module General.General, trace) where

import Data.List
import Control.Monad
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


ungroupKey :: [(key,[val])] -> [(key,val)]
ungroupKey = concatMap (\(k,v) -> map ((,) k) v)


on :: (new -> new -> res) -> (orig -> new) -> (orig -> orig -> res)
on f g x y = f (g x) (g y)


concatMapM f xs = liftM concat $ mapM f xs

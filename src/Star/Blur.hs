
module Star.Blur(blur) where

import Star.Type
import Options
import List


blur :: (Eq a, Show a) => Star a -> Star a
blur x = if starSize res > 10 then fullBlur x else res
    where
        res = mapStar f x
    
        f (Star x z b) | any (>= pathBlurFrom) z =
            star x (if null res then [pathBlurTo] else res) True
            where res = filter (<= pathBlurTo) z

        f x = x


-- | Completely blur, only in the worst case!
fullBlur :: (Eq a, Show a) => Star a -> Star a
fullBlur x = Star (StarUni $ map StarLit $ nub $ allStarLit x) [0] True

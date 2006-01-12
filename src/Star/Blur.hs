
module Star.Blur(blur) where

import Star.Type


blur :: (Eq a, Show a) => Star a -> Star a
blur x = mapStar f x
    where
        f (Star x z b) = star x
                         (if null res then [3] else res)
                         (b || any (>= 3) z)
            where res = filter (< 3) z
        f x = x

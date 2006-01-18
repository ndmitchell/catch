
module Star.Blur(blur) where

import Star.Type
import Options


blur :: (Eq a, Show a) => Star a -> Star a
blur x = mapStar f x
    where
        f (Star x z b) | any (>= pathBlurFrom) z =
            star x (if null res then [pathBlurTo] else res) True
            where res = filter (<= pathBlurTo) z

        f x = x

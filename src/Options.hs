{-|
    Various options that can be controlled statically
    Usually used for debugging
-}

module Options where



-- | Path blurring
-- At x^{pathBlurFrom}, set it to x^{pathBlurTo..\inf}
pathBlurFrom = 2 :: Int
pathBlurTo = 1 :: Int


-- | Expression blurring
-- At x^{hiteBlurFrom}, set it to x^{hiteBlurTo..\inf}
hiteBlurFrom = 2 :: Int
hiteBlurTo = 1 :: Int
hiteBlurMax = 5 :: Int

-- | Maximum number of lines of computation
maxCompute = 100000 :: Int


-- | Should the propagation step take into account the branches
propagateSimp = False


simplifyRegular = True

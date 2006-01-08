{- |
	QuickCheck properties for regular expressions.
	Designed to keep the tests correct.
	In general on quickCheck error fire an internal error and give more error info.
-}

module RegExpTest where

--import Test.QuickCheck
import QuickCheck
import Monad

import RegExp
import Fsa
import FsaGraph
import General


---------------------------------------------------------------------
-- * Generation Section

instance Arbitrary Char where
    arbitrary = oneof (map return "abcd")

instance Arbitrary a => Arbitrary (RegExp a) where
    arbitrary = complex
        where
            complex = oneof
                [liftM RegKleene simple, liftM RegConcat (list2 simple simple), liftM RegUnion (list2 simple simple)]
        
            simple = oneof (replicate 4 (liftM RegLit arbitrary) ++ 
                [return RegOmega, 
                 liftM RegKleene simple,
                 liftM RegConcat (list2 simple simple),
                 liftM RegUnion (list2 simple simple)
                 ])

            list2 = liftM2 list2_make
            list2_make a b = [a,b]


-- | A data structure being a list of length 'listLength'
data ListRegExp a = ListRegExp [RegExp a]
                  deriving (Show)

-- | The length of the 'ListRegExp', set to 100
listLength :: Int
listLength = 100

instance Arbitrary a => Arbitrary (ListRegExp a) where
    arbitrary = liftM ListRegExp (vector listLength)


ensure True _ = True
ensure False x = error $ show x

ensureBool b _ = b


-- | Find a minimal failing example.
findMin :: (RegExpChar -> Bool) -> RegExpChar -> RegExpChar
findMin prop ex = if null fails then ex else findMin prop (head fails)
	where fails = filter (not . prop) (smaller ex)

---------------------------------------------------------------------
-- * Regular Expression Properties


-- | Used for generating a list for visual inspection.
--   Always fires an error.
propOutput :: ListRegExp Char -> Bool
propOutput (ListRegExp xs) = error $ unlines $ map show xs


-- | Repeated calls to 'reduce' have no effect.
--   Should be TRUE.
propReduceFixedPoint :: RegExpChar -> Bool
propReduceFixedPoint x = redx ~= reduce redx
    where redx = reduce x
    

-- | Calling 'reduce' should result in an equal expression.
--   Should be TRUE.
propReduceEq :: RegExpChar -> Bool
propReduceEq x = ensure (redx == x) (x, redx)
	where redx = reduce x

---------------------------------------------------------------------
-- * State Machine Properties

-- | Roundtripping should work (not a great test, because of Fsa equality)
--   Should be TRUE.
propFsaInputOutputEq :: RegExpChar -> Bool
propFsaInputOutputEq x = x == fsaOutputDet (fsaInput x)

-- | Do you get the original expression back on reg -> fsa -> reg.
--   FALSE, i.e. a+b*
propFsaInputOutputSimilar :: RegExpChar -> Bool
propFsaInputOutputSimilar x = ensure (left ~= right) (left, right)
    where
        left = reduce x
        right = fsaOutputDet (fsaInput x)


-- | Does 'fsaInput' then 'fsaOutputMin' generate an equal expression.
--   Should be TRUE.
propFsaInputOutputMinEq :: RegExpChar -> Bool
propFsaInputOutputMinEq x = x2 == fsaOutputMinDet (fsaInput x2)
    where x2 = reduce x

-- | Does 'fsaInput' then 'fsaOutputMin' generate a similar expression.
--   FALSE, but a good testbed.
propFsaInputOutputMinSimilar :: RegExpChar -> Bool
propFsaInputOutputMinSimilar x = ensure (left ~= right) (left, right)
    where
        left = reduce x
        right = fsaOutputMinDet (fsaInput left)


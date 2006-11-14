
import Data.Proposition
import Test.QuickCheck
import Control.Monad



data PropTest = PropTest Char deriving (Eq, Ord)

instance Show PropTest where
    show (PropTest x) = [x]


instance PropLit PropTest


instance Arbitrary PropTest where
    arbitrary = oneof $ map (return . PropTest) ['a'..'f']


instance (PropLit a, Arbitrary a) => Arbitrary (PropSimple a) where
    arbitrary = sized f
        where
            f 0 = liftM propLit arbitrary
            f n = oneof [liftM id half, liftM2 propAnd half half, liftM2 propOr half half]
                where half = f (n `div` 2)


test :: PropSimple PropTest -> Bool
test x = error $ show x

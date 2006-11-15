
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


type Test = PropSimple PropTest


test :: Test -> Bool
test simp = eval simp == eval bdd
    where bdd = propRebuild simp :: BDD PropTest


testAnd :: Test -> Test -> Bool
testAnd a b = eval (a `propAnd` b) == eval (a2 `propAnd` b2)
    where
        a2 = propRebuildBDD a
        b2 = propRebuildBDD b

testOr :: Test -> Test -> Bool
testOr a b = eval (a `propOr` b) == eval (a2 `propOr` b2)
    where
        a2 = propRebuildBDD a
        b2 = propRebuildBDD b

testMap :: Test -> Bool
testMap a = eval (propMap f a) == eval (propMap f $ propRebuildBDD a)
    where
        rep = [('a','f'),('f','a')]
        f (PropTest x) = propLit $ case lookup x rep of
                             Nothing -> PropTest x
                             Just y -> PropTest y




eval :: Prop p => p PropTest -> Bool
eval prop = if propIsTrue res then True
            else if propIsFalse res then False
            else error "Proposition.eval does not return a boolean"
    where
        res = propMap f prop
        f (PropTest x) = propBool $ x <= 'c'

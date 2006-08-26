
module Data.BDD(BDD, bddAnd, bddOr, bddLit, bddAnds, bddOrs, bddIsTrue, bddIsFalse) where


data BDD a = AtomTrue
		   | AtomFalse
		   | Choice a (BDD a) (BDD a) -- false, true
		   deriving Eq


bddLit :: a -> BDD a
bddLit a = Choice a AtomFalse AtomTrue

bddAnds x = foldr bddAnd AtomTrue  x
bddOrs  x = foldr bddOr  AtomFalse x


bddAnd :: Ord a => BDD a -> BDD a -> BDD a
bddAnd AtomTrue b = b
bddAnd b AtomTrue = b
bddAnd AtomFalse b = AtomFalse
bddAnd b AtomFalse = AtomFalse
bddAnd a b = mergeWith bddAnd a b


bddOr :: Ord a => BDD a -> BDD a -> BDD a
bddOr AtomFalse b = b
bddOr b AtomFalse = b
bddOr AtomTrue b = AtomTrue
bddOr b AtomTrue = AtomTrue
bddOr a b = mergeWith bddOr a b


mergeWith :: Ord a => (BDD a -> BDD a -> BDD a) -> BDD a -> BDD a -> BDD a
mergeWith f c1@(Choice a1 f1 t1) c2@(Choice a2 f2 t2) =
	case compare a1 a2 of
		EQ -> choice a1 (f f1 f2) (f t1 t2)
		LT -> choice a1 (f f1 c2) (f t1 c2)
		GT -> choice a1 (f c1 f2) (f c1 t2)

choice a t f = if t == f then t else Choice a t f


bddIsTrue :: BDD a -> Bool
bddIsTrue AtomTrue = True
bddIsTrue _ = False

bddIsFalse :: BDD a -> Bool
bddIsFalse AtomFalse = True
bddIsFalse _ = False



module Data.BDD where
    

data BDD a = AtomTrue
		   | AtomFalse
		   | Choice a (BDD a) (BDD a) -- false, true
		   deriving (Eq, Ord)



bddValid :: Ord a => BDD a -> Bool
bddValid (Choice a f1 t1) = f a f1 && f a t1
	where
		f x (Choice a f1 t1) = x < a && f a f1 && f a t1
		f x _ = True
	
bddValid _ = True



choice a t f = if t == f then t else Choice a t f


{-
propRebal :: BDD Char -> Bool
propRebal x = hasBalance $ rebalance x
-}


hasBalance (Choice a f t) = check a f && check a t
    where
        check a0 (Choice a f t) = a0 < a && check a f && check a t
        check a0 _ = True
hasBalance _ = True


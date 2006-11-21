
module Data.BDD where
    

data BDD a = AtomTrue
		   | AtomFalse
		   | Choice a (BDD a) (BDD a) -- false, true
		   deriving (Eq, Ord)


instance Show a => Show (BDD a) where
    show AtomTrue = "True"
    show AtomFalse = "False"
    show (Choice a AtomFalse AtomTrue) = show a
    show (Choice a AtomTrue AtomFalse) = "~" ++ show a
    show (Choice a f t) = show a ++ " <" ++ show f ++ " | " ++ show t ++ ">"


bddValid :: Ord a => BDD a -> Bool
bddValid (Choice a f1 t1) = f a f1 && f a t1
	where
		f x (Choice a f1 t1) = x < a && f a f1 && f a t1
		f x _ = True
	
bddValid _ = True


bddApplyAnd :: (Show a, Ord a) => (a -> a -> Maybe a) -> BDD a -> BDD a
bddApplyAnd merge = rebalance . f
	where
		f (Choice on1 false1 true1@(Choice on2 false2 true2))
			| false1 == false2 = case merge on1 on2 of
									 Just on -> Choice on  (f false2) (f true2)
									 Nothing -> Choice on1 (f false1) (f true1)
		f (Choice on false true) = Choice on (f false) (f true)
		f x = x



choice a t f = if t == f then t else Choice a t f


propRebal :: BDD Char -> Bool
propRebal x = hasBalance $ rebalance x


hasBalance (Choice a f t) = check a f && check a t
    where
        check a0 (Choice a f t) = a0 < a && check a f && check a t
        check a0 _ = True
hasBalance _ = True



data Focus = FLeft | FRight | FBoth | FNone


rebalance :: (Show a, Ord a) => BDD a -> BDD a
rebalance AtomTrue = AtomTrue
rebalance AtomFalse = AtomFalse
rebalance (Choice a f t) = {- assert (hasBalance res) $ -} res
	where
        res = g $ Choice a (rebalance f) (rebalance t)
        
        g (Choice a f t) = case focus of
                FNone -> choice a f t
                FBoth ->
                    case compare a af of
                        EQ -> choice a ff tt
                        LT -> choice a f t
                        GT -> choice af (g $ Choice a ff tf) (g $ Choice a ft tt)
                FLeft ->
                    case compare a af of
                        EQ -> choice a ff t
                        LT -> choice a f t
                        GT -> choice af (g $ Choice a ff t) (g $ Choice a ft t)
                FRight ->
                    case compare a at of
                        EQ -> choice a f tt
                        LT -> choice a f t
                        GT -> choice at (g $ Choice a f tf) (g $ Choice a f tt)
            where
                Choice af ff ft = f
                Choice at tf tt = t
            
                focus = case (f,t) of
                            (Choice a _ _, Choice b _ _) -> case compare a b of {EQ -> FBoth; LT -> FLeft; GT -> FRight}
                            (Choice a _ _, _) -> FLeft
                            (_, Choice b _ _) -> FRight
                            _ -> FNone


bddSimplify :: Ord a => ([(a,Bool)] -> a -> Maybe Bool) -> BDD a -> BDD a
bddSimplify test x = f [] x
	where
		f context (Choice on false true) =
			case test context on of
				Nothing -> choice on (f ((on,False):context) false) (f ((on,True):context) true)
				Just b -> f context (if b then true else false)
		f _ x = x

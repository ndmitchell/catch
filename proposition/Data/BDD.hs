
module Data.BDD(BDD, bddAnd, bddNot, bddOr, bddLit,
	bddIsTrue, mapBDDM, bddIsFalse, bddTrue, bddFalse, bddSimplify,
	bddApplyAnd) where
    
import qualified Data.Map as Map


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


bddLit :: a -> BDD a
bddLit a = Choice a AtomFalse AtomTrue


bddNot :: (Show a, Ord a) => BDD a -> BDD a
bddNot AtomTrue  = AtomFalse
bddNot AtomFalse = AtomTrue
bddNot (Choice a f t) = Choice a (bddNot f) (bddNot t)


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
mergeWith f c1 c2 | not (bddValid c1) || not (bddValid c2) = error "Precond failed"
mergeWith f c1@(Choice a1 f1 t1) c2@(Choice a2 f2 t2) = 
	if bddValid res then res else error $ "Postcond failed" ++ show (compare a1 a2)
 where
  res =
	case compare a1 a2 of
		EQ -> choice a1 (f f1 f2) (f t1 t2)
		LT -> choice a1 (f f1 c2) (f t1 c2)
		GT -> choice a2 (f c1 f2) (f c1 t2)

choice a t f = if t == f then t else Choice a t f


bddIsTrue :: BDD a -> Bool
bddIsTrue AtomTrue = True
bddIsTrue _ = False

bddIsFalse :: BDD a -> Bool
bddIsFalse AtomFalse = True
bddIsFalse _ = False


bddTrue = AtomTrue
bddFalse = AtomFalse


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


getSize (Choice _ f t) = getSize f + getSize t + 1
getSize _ = 1


mapBDDM :: (Show a, Monad m, Ord a) => (a -> m (BDD a)) -> BDD a -> m (BDD a)
mapBDDM app x = do
        (d, res) <- g (appWrap app) x Map.empty
        return $ rebalance res

appWrap app x = app x

g app (Choice a f0 t0) cache = do
    (cache,a2) <- case Map.lookup a cache of
        Just a2 -> return (cache,a2)
        Nothing -> do
            a2 <- app a
            return (Map.insert a a2 cache,a2)

    case a2 of
        AtomTrue -> g app t0 cache
        AtomFalse -> g app f0 cache
        Choice a2 f1 t1 -> do
            (cache,f0) <- g app f0 cache
            (cache,t0) <- g app t0 cache
            return (cache, Choice a2 (h f1 f0 t0) (h t1 f0 t0))


g app x cache = return (cache,x)

-- replace all occurances of AtomTrue/AtomFalse with the given predicate
h rep f t = case rep of
    AtomTrue -> t
    AtomFalse -> f
    Choice a f1 t1 -> Choice a (h f1 f t) (h t1 f t)

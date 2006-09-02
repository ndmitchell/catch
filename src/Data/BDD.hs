
module Data.BDD(BDD, {-BDDLit(..), -} showBDDBy, bddAnd, bddNot, bddOr, bddLit, bddAnds, bddOrs,
	bddIsTrue, mapBDDM, bddIsFalse, mapBDD, bddBool, bddTrue, bddFalse, bddSimplify) where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad
import Control.Monad.Identity


data BDD a = AtomTrue
		   | AtomFalse
		   | Choice a (BDD a) (BDD a) -- false, true
		   deriving (Eq, Ord, Show)


class (Show a, Ord a) => BDDLit a where
	litNot :: a -> BDD a


showBDDBy :: (a -> String) -> BDD a -> String
showBDDBy f AtomTrue = "True"
showBDDBy f AtomFalse = "False"
showBDDBy f (Choice a AtomFalse AtomTrue) = f a
showBDDBy f (Choice a f1 t1) = f a ++ " <" ++ showBDDBy f f1 ++ " | " ++ showBDDBy f t1 ++ ">"


bddValid :: Ord a => BDD a -> Bool
bddValid (Choice a f1 t1) = f a f1 && f a t1
	where
		f x (Choice a f1 t1) = x < a && f a f1 && f a t1
		f x _ = True
	
bddValid _ = True


bddLit :: a -> BDD a
bddLit a = Choice a AtomFalse AtomTrue

bddAnds x = foldr bddAnd AtomTrue  x
bddOrs  x = foldr bddOr  AtomFalse x


bddNot :: Ord a => (a -> a) -> BDD a -> BDD a
bddNot invert = rebalance . swap
	where
		swap AtomFalse = AtomTrue
		swap AtomTrue  = AtomFalse
		swap (Choice a f t) = Choice (invert a) (swap t) (swap f)

--bddNot AtomFalse = AtomTrue
--bddNot AtomTrue  = AtomFalse
--bddNot c = error $ showBDDBy show c ++ " ==> " ++ showBDDBy show (mapBDD litNot c)
{-	where
		g (Choice a f t) = case litNot a of
							   
		
		Choice a (g t) (g f)
		g x = x
-}


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


bddBool True = AtomTrue
bddBool False = AtomFalse

bddTrue = AtomTrue
bddFalse = AtomFalse


rebalance :: Ord a => BDD a -> BDD a
rebalance AtomTrue = AtomTrue
rebalance AtomFalse = AtomFalse
rebalance (Choice a f t) = g (Choice a f2 t2)
	where
		f2 = rebalance f
		t2 = rebalance t
		
		g (Choice a f (Choice ta tf tt))
			| a == ta = g $ Choice a f tt
			| a >  ta = g $ Choice ta (g $ Choice a f tf) (g $ Choice a f tt)

		g (Choice a (Choice fa ff ft) t)
			| a == fa = g $ Choice a ff t
			| a >  fa = g $ Choice fa (g $ Choice a ff t) (g $ Choice a ft t)
		
		g (Choice a t f)
			| t == f = g $ t
		
		g x = x


bddSimplify :: Ord a => ([(a,Bool)] -> a -> Maybe Bool) -> BDD a -> BDD a
bddSimplify test x = f [] x
	where
		f context (Choice on false true) =
			case test context on of
				Nothing -> choice on (f ((on,False):context) false) (f ((on,True):context) true)
				Just b -> f context (if b then true else false)
		f _ x = x


{-

mapBDD :: BDDLit a => (a -> BDD a) -> BDD a -> BDD a
mapBDD f x = rebalance $ snd $ g x Map.empty
	where
		g (Choice a f1 t1) cache = case a2 of
				AtomTrue -> g t1 cache2
				AtomFalse -> g f1 cache2
				Choice a2 f2 t2 ->
					let (cache3,f3) = g f1 cache2
					    (cache4,t3) = g t1 cache3
					in (cache4, g Choice a2 (h f2 (g f1)
			where
				h
				
				
		
			Choice a f1 t1 -> 
			
-}


mapBDD :: Ord a => (a -> BDD a) -> BDD a -> BDD a
mapBDD f = runIdentity . mapBDDM (return . f)


mapBDDM :: (Monad m, Ord a) => (a -> m (BDD a)) -> BDD a -> m (BDD a)
mapBDDM app x = do
		(_, res) <- g x Map.empty
		return $ rebalance res
	where
		g (Choice a f0 t0) cache = do
			(cache,a2) <- case Map.lookup a cache of
				Just a2 -> return (cache,a2)
				Nothing -> do
					a2 <- app a
					return (Map.insert a a2 cache,a2)
			
			case a2 of
				AtomTrue -> g t0 cache
				AtomFalse -> g f0 cache
				Choice a2 f1 t1 -> do
					(cache,f0) <- g f0 cache
					(cache,t0) <- g t0 cache
					return (cache, Choice a2 (h f1 f0 t0) (h t1 f0 t0))
		
		g x cache = return (cache,x)
		
		-- replace all occurances of AtomTrue/AtomFalse with the given predicate
		h rep f t = case rep of
			AtomTrue -> t
			AtomFalse -> f
			Choice a f1 t1 -> Choice a (h f1 f t) (h t1 f t)

{-
case x of
	AtomTrue -> AtomTrue
	AtomFalse -> AtomFalse
	Choice a f1 t1 ->
		let a2 = f a
		in bddAnd a2 (mapBDD f t1) `bddOr` bddAnd (bddNot a2) (mapBDD f f1)


mapBDDM :: (Show a, BDDLit a) => (a -> IO (BDD a)) -> BDD a -> IO (BDD a)
mapBDDM app x = do
		cache <- newIORef Map.empty
		g cache x
	where
		g cache x = case x of
			Choice a f1 t1 -> do
				c <- readIORef cache
				a2 <- case Map.lookup a c of
					Just a2 -> return a2
					Nothing -> do
						a2 <- app a
						writeIORef cache (Map.insert a a2 c)
						return a2

				case () of
					_ | bddIsTrue  a2 -> g cache t1
					_ | bddIsFalse a2 -> g cache f1
					_ -> do
						t2 <- liftM (bddAnd a2) $ g cache t1
						if bddIsTrue t2 then return t2 else do
							f2 <- liftM (bddAnd (bddNot a2)) $ g cache f1
							return $ t2 `bddOr` f2

			_ -> return x
-}

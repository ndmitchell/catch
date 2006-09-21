
module Data.BDD(BDD, {-BDDLit(..), -} showBDDBy, bddAnd, bddNot, bddOr, bddLit, bddAnds, bddOrs,
	bddIsTrue, mapBDDM, bddIsFalse, mapBDD, bddBool, bddTrue, bddFalse, bddSimplify,
	bddApplyAnd) where
    
import Debug.Trace
import General.General
import qualified Data.Map as Map
import Data.IORef
import Control.Monad
import Control.Monad.Identity
import Control.Exception


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

bddAnds x = foldr bddAnd AtomTrue  x
bddOrs  x = foldr bddOr  AtomFalse x


bddNot :: (Show a, Ord a) => (a -> a) -> BDD a -> BDD a
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
        
        {-
        
        
        
        balBoth (Choice a f t) | t == f = t
        balBoth o@(Choice a (Choice fa ff ft) (Choice ta tf tt)) | fa == ta =
            case compare a fa of
                EQ -> Choice a ff tt
                LT -> o
                GT -> Choice fa (g $ Choice a ff tf) (g $ Choice a ft tt)
                
                
        balBoth x = balLeft x
        
        balLeft o@(Choice a (Choice fa ff ft) t) =
            case compare a fa of
                EQ -> g $ Choice a ff t
                LT -> balRight o
                GT -> Choice fa (
        
        -}
        
        --g 0 x | trace ("rebal.g " ++ showBDDBy show x) False = undefined

        -- g n x | trace ("Rebalance.g " ++ show n {- showBDDBy show x-} ) False = undefined
        {-
        g n (Choice a t f)
            | t == f = t

        g n (Choice a f (Choice ta tf tt))
            | a == ta = k "==" a ta $ g 1 $ Choice a f tt
            | a >  ta = k ">" a ta $ g 2 $ Choice ta (g 3 $ Choice a f tf) (g 4 $ Choice a f tt)

        g n (Choice a (Choice fa ff ft) t)
            | a == fa = k "==" a fa $ g 5 $ Choice a ff t
            | a >  fa = k ">" a fa $ g 6 $ Choice fa (g 7 $ Choice a ff t) (g 8 $ Choice a ft t)

        g n x = x
        
        k rel a b = trace (rel ++ " " ++ show a ++ " ### " ++ show b)
        -}


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


mapBDD :: (Show a, Ord a) => (a -> BDD a) -> BDD a -> BDD a
mapBDD f = runIdentity . mapBDDM (return . f)


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

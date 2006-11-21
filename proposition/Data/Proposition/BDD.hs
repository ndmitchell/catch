
module Data.Proposition.BDD(BDD) where

import Data.Proposition.Internal
import qualified Data.Map as Map


data BDD a = AtomTrue
           | AtomFalse
           | Choice a (BDD a) (BDD a) -- false, true
           deriving (Eq, Ord)


instance Prop BDD where
    propTrue  = AtomTrue
    propFalse = AtomFalse
    
    propIsTrue  AtomTrue  = True; propIsTrue  _ = False
    propIsFalse AtomFalse = True; propIsFalse _ = False

    propLit a = Choice a AtomFalse AtomTrue

    propNot AtomTrue  = AtomFalse
    propNot AtomFalse = AtomTrue
    propNot (Choice a f t) = Choice a (propNot f) (propNot t)

    propAnd = mergeWith propIsTrue  propIsFalse 
    propOr  = mergeWith propIsFalse propIsTrue

    propMapM = mapMonadic
    propFold = fold

    propSimplify = simplifyImplies . simplifyAnd


instance Show a => Show (BDD a) where
    show AtomTrue = "True"
    show AtomFalse = "False"
    show (Choice a AtomFalse AtomTrue) = show a
    show (Choice a AtomTrue AtomFalse) = "~" ++ show a
    show (Choice a f t) = show a ++ " <" ++ show f ++ " | " ++ show t ++ ">"


---------------------------------------------------------------------
-- MERGING, FOR OR/AND

choice a t f = if t == f then t else Choice a t f


mergeWith :: Ord a => (BDD a -> Bool) -> (BDD a -> Bool) -> BDD a -> BDD a -> BDD a
mergeWith ignore promote c1 c2
    | ignore c1 = c2
    | ignore c2 = c1
    | promote c1 = c1
    | promote c2 = c2

mergeWith ignore promote c1@(Choice a1 f1 t1) c2@(Choice a2 f2 t2) =
    case compare a1 a2 of
        EQ -> choice a1 (cont f1 f2) (cont t1 t2)
        LT -> choice a1 (cont f1 c2) (cont t1 c2)
        GT -> choice a2 (cont c1 f2) (cont c1 t2)
    where
        cont = mergeWith ignore promote


---------------------------------------------------------------------
-- MAPPING

mapMonadic :: (Show a, Monad m, Ord a) => (a -> m (BDD a)) -> BDD a -> m (BDD a)
mapMonadic app x = do
        (d, res) <- g app x Map.empty
        return $ rebalance res
    where
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
                    return (cache, Choice a2 (replaceBools f1 f0 t0) (replaceBools t1 f0 t0))

        g app x cache = return (cache,x)

        -- replace all occurances of AtomTrue/AtomFalse with the given predicate
        replaceBools rep f t = case rep of
            AtomTrue -> t
            AtomFalse -> f
            Choice a f1 t1 -> Choice a (replaceBools f1 f t) (replaceBools t1 f t)



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


---------------------------------------------------------------------
-- FOLDING

fold :: PropFold a res -> BDD a -> res
fold fs AtomTrue  = foldAnd fs []
fold fs AtomFalse = foldOr  fs []

-- (a => t) ^ (¬a => f)
-- (¬a v t) ^ (a  v  f)
fold fs (Choice a f t) = (not a2 || fold fs t) && (a2 || fold fs f)
    where
        a2 = foldLit fs a
        not = foldNot fs
        (||) a b = foldOr  fs [a,b]
        (&&) a b = foldAnd fs [a,b]



---------------------------------------------------------------------
-- SIMPLIFICATION

simplifyImplies :: PropLit a => BDD a -> BDD a
simplifyImplies x = f [] x
    where
        f context (Choice on false true) =
            case context ?=> on of
                Nothing -> choice on (f ((on,False):context) false) (f ((on,True):context) true)
                Just b -> f context (if b then true else false)
        f _ x = x


simplifyAnd :: PropLit a => BDD a -> BDD a
simplifyAnd = rebalance . f
    where
        f (Choice on1 false1 true1@(Choice on2 false2 true2))
            | false1 == false2 =
              case on1 ?/\ on2 of
                  Value on -> Choice on  (f false2) (f true2)
                  Literal b -> propBool b
                  _ -> Choice on1 (f false1) (f true1)
                                     
        f (Choice on false true) = Choice on (f false) (f true)
        f x = x


---------------------------------------------------------------------
-- CHECKS

isValid :: Ord a => BDD a -> Bool
isValid = isBalanced


isBalanced :: Ord a => BDD a -> Bool
isBalanced (Choice a f t) = check a f && check a t
    where
        check a0 (Choice a f t) = a0 < a && check a f && check a t
        check a0 _ = True
isBalanced _ = True




module Typey.Type where

import Hite
import General.General
import Data.Maybe
import Data.List
import Control.Exception


-- SHOW STUFF

instance Show Large2T where
    show x = f 0 x
        where
            f n (Free2T i) = i
            f n (Ctor2T x) = x
            f n (Bind2T x xs) = brack (n>=2) $ show x ++ concatMap ((' ':) . f 2) xs
            f n (Arr2T xs x) = brack (n>=1) $ intercat " -> " $ map (f 1) (xs++[x])

            brack b x = ['('|b] ++ x ++ [')'|b]

instance Show FuncT where
    show (FuncT n args res) = "forall " ++ show n ++ " . " ++ (concat $ intersperse " -> " $ map show $ args ++ [res])
    
instance Show LargeT where
    show (FreeL i) = "#" ++ show i
    show (CtorL name args) = "(" ++ name ++ concatMap ((' ':) . show) args ++ ")"

instance Show SmallT where
    show (FreeS i) = "#" ++ show i
    show Self = "*"

instance Show a => Show (DataT a) where
    show (DataT n xs) = "forall " ++ show n ++ " . " ++ (concat $ intersperse " | " $ map show xs)

instance Show a => Show (CtorT a) where
    show (CtorT name xs) = name ++ concatMap ((' ':) . show) xs


instance Show Subtype where
    showList xs = showString $ concat $ intersperse " -> " $ map show xs

    show (Subtype a b) = "Subtype" ++ show a ++ show b
    show (Atom vals) = show vals
    show Empty = "Empty"

instance Show Subpair where
    show ([] :@ _) = ""
    show (a :@ b) = " <" ++ show a ++ " @ " ++ show b ++ ">"

instance Show Subvalue where
    showList [] = showString "?"
    showList [x] = showString $ show x
    showList xs = showString $ "[" ++ (concat $ intersperse "," $ map show xs) ++ "]"

    show Bot = "_|_"
    show (SVar n) = "#" ++ show n
    show (SCtor s) = s


-- TYPE STUFF

type FuncM = [(FuncName, FuncT)]
type Func2M = [(FuncName, Large2T)]
type DataM a = [(DataName, DataT a)]

data FuncT = FuncT Int [LargeT] LargeT
data DataT a = DataT Int [CtorT a]
data CtorT a = CtorT String [a]
data LargeT = FreeL Int | CtorL String [LargeT]
data SmallT = FreeS Int | Self

data Large2T = Ctor2T String
             | Free2T String
             | Bind2T Large2T [Large2T]
             | Arr2T [Large2T] Large2T

mapLarge2T :: (Large2T -> Large2T) -> Large2T -> Large2T
mapLarge2T f x = f $ case x of
        Bind2T x xs -> Bind2T (mapLarge2T f x) (map (mapLarge2T f) xs)
        Arr2T a b -> Arr2T (map (mapLarge2T f) a) (mapLarge2T f b)
        x -> x

isHigherOrder :: Large2T -> Bool
isHigherOrder (Arr2T _ _) = True
isHigherOrder (Bind2T x xs) = any isHigherOrder (x:xs)
isHigherOrder _ = False


isRecursive (CtorT name xs) = any isSelf xs
isSelf (Self) = True; isSelf _ = False


class PlayLargeT a where
    mapLargeT :: (LargeT -> LargeT) -> a -> a
    
instance PlayLargeT LargeT where
    mapLargeT f x = f $ case x of
                        CtorL name xs -> CtorL name $ map (mapLargeT f) xs
                        x -> x

instance PlayLargeT FuncT where
    mapLargeT f (FuncT n xs x) = FuncT n (map (mapLargeT f) xs) (mapLargeT f x)



-- SUBTYPE STUFF

data Subtype = Subtype Subpair Subpair
             | Atom [Subvalue] -- contains Top, Bot, Var (no Ctor)
             | Empty

-- the constructors at this level, and inside them
data Subpair = [Subvalue] :@ [Subtype] -- first one is Ctor

data Subvalue = Bot
              | SVar Int
              | SCtor String
                deriving Eq


getSubtypesData :: DataT SmallT -> [Subtype]
getSubtypesData (DataT n ctors) = concatMap f ctors
    where
        f c@(CtorT name _) | not (isRecursive c) = [Subtype ([SCtor name] :@ []) ([] :@ [])]
                           | otherwise = [Subtype ([SCtor name] :@ []) ([SCtor x] :@ []) | CtorT x _ <- ctors]

getSubtypesLarge :: DataM SmallT -> LargeT -> [Subtype]
getSubtypesLarge datam (FreeL n) = [Atom []]
getSubtypesLarge datam (CtorL name args) = concatMap f datat
    where
        f (Subtype (a :@ []) (b :@ []))
            | null b = [Subtype (a :@ x) ([] :@ []) | x <- children]
            | otherwise = [Subtype (a :@ x) (b :@ y) | x <- children, y <- children]
    
        datat = getSubtypesData $ lookupJust name datam
        children = crossProduct $ map (getSubtypesLarge datam) args

getSubtypesFunc :: DataM SmallT -> FuncT -> [[Subtype]]
getSubtypesFunc datam (FuncT n args res) = crossProduct $ map (getSubtypesLarge datam) args



-- emptySubtype = Subtype [] [] [] []


class Union a where
    unionPair :: a -> a -> a
    unionList :: [a] -> a
    
    unionList [] = error "unionList, with empty list"
    unionList xs = foldr1 unionPair xs


unionListNote msg [] = error $ "unionListNote, " ++ msg
unionListNote msg x = unionList x

instance Union Subtype where
    unionPair (Subtype a1 b1) (Subtype a2 b2) = Subtype (unionPair a1 a2) (unionPair b1 b2)
    unionPair (Atom a) (Atom b) = Atom $ unionSubvalues a b
    unionPair Empty x = x
    unionPair x Empty = x

instance Union Subpair where
    unionPair (a1 :@ b1) (a2 :@ b2) = (unionSubvalues a1 a2 :@ zipWithRest unionPair b1 b2)

unionSubvalues :: [Subvalue] -> [Subvalue] -> [Subvalue]
unionSubvalues as bs = as `union` bs


hasBottom :: Subtype -> Bool
hasBottom (Atom xs) = Bot `elem` xs
hasBottom (Subtype a b) = f a || f b
    where f (a :@ b) = hasBottom (Atom a) || any hasBottom b
hasBottom Empty = False

allBottom :: Subtype -> Subtype
allBottom (Atom x) = Atom [Bot]
allBottom (Subtype a b) = Subtype (f a) (f b)
    where f (a :@ b) = (if null a then [] else [Bot]) :@ map allBottom b

-- drop all items to the bottom (recursive) level
collapseSubtype :: Subtype -> Subtype
collapseSubtype (Subtype a b) = Subtype ([] :@ []) (unionPair a b)

liftSubtype :: Subtype -> Subtype
liftSubtype (Subtype a b) = Subtype b b

isSubset :: Subtype -> Subtype -> Bool
isSubset s1@(Subtype a1 b1) s2@(Subtype a2 b2) = f a1 a2 && f b1 b2
    where
        f ([]:@_) _ = True
        f (a1:@b1) (a2:@b2) = g a1 a2 && (and $ zipWithEqNote ("Typey.Type.isSubset " ++ show (s1,s2)) isSubset b1 b2)
        g x y = null (x \\ y)
isSubset (Atom _) (Atom _) = True
isSubset (Atom _) (Subtype _ _) = True -- a free variable can be a subtype



module Train.Path(Path, nullPath, newPath, ewpPath, emptyPath, finitePath, makeFinitePath, integrate, differentiate) where

import General.General
import Data.Char
import Hite
import Control.Monad


data Path = Path Hite [PathElem]
			deriving Show

instance Eq Path where
	(Path _ a) == (Path _ b) = a == b

instance Ord Path where
	compare (Path _ a) (Path _ b) = compare a b


data PathElem = PathAtom CtorArg
	          | PathStar CtorArg
	          deriving (Eq, Show, Ord)


instance Output Path where
	output (Path _ xs) = concatMap (('.':) . map toUpper . output) xs

instance Output PathElem where
	output (PathAtom x) = x
	output (PathStar x) = x ++ "*"


isPathStar (PathStar{}) = True ; isPathStar _ = False
isPathAtom = not . isPathStar


nullPath (Path _ x) = null x

emptyPath hite = Path hite []

newPath hite xs = foldl integrate (emptyPath hite) xs


ewpPath (Path _ x) = all isPathStar x

finitePath (Path _ x) = all (not . isPathStar) x
makeFinitePath (Path hite x) = Path hite $ filter (not . isPathStar) x


isStar hite x = TyCon (dataName obj) (map TyFree $ frees obj) == cargType obj
	where obj = getCArg hite x


differentiate :: Path -> CtorArg -> Maybe Path
differentiate (Path hite xs) ctor = liftM (Path hite) $ f xs
	where
		f [] = Nothing
		f (PathAtom x:xs) | x == ctor = Just xs
						  | otherwise = Nothing
		f (PathStar x:xs) | x == ctor = Just (PathStar x:xs)
						  | otherwise = f xs
						  


integrate :: Path -> CtorArg -> Path
integrate (Path hite x) ctor = Path hite (f x)
	where
		f ys@(PathStar y:_) | ctor == y = ys
		f ys = (if isStar hite ctor then PathStar ctor else PathAtom ctor) : ys




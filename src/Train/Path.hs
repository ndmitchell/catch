
module Train.Path(Path, nullPath, emptyPath, integrate) where

import General.General
import Data.Char
import Hite


data Path = Path [PathElem]
			deriving (Eq, Show)


data PathElem = PathAtom CtorArg
	          | PathStar CtorArg
	          deriving (Eq, Show)


instance Output Path where
	output (Path xs) = concatMap (('.':) . map toUpper . output) xs

instance Output PathElem where
	output (PathAtom x) = x
	output (PathStar x) = x ++ "*"


nullPath (Path x) = null x

emptyPath = Path []



isStar x = x `elem` ["tl"]

integrate :: Path -> CtorArg -> Path
integrate (Path x) ctor = Path (f x)
	where
		f ys@(PathStar y:_) | ctor == y = ys
		f ys = (if isStar ctor then PathStar ctor else PathAtom ctor) : ys




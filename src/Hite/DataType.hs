
module Hite.DataType where

import Hite.TypeType
import General.General
import Data.List


type DataName = String
type CtorName = String
type CtorArg  = String


type Datas = [Data]

data Data = Data {dataName :: DataName, ctors :: [Ctor], frees :: [String]}
            deriving (Show, Read)

data Ctor = Ctor {ctorName :: CtorName, ctorArgs :: [CtorArg], types :: [TyType]}
          deriving (Eq, Show, Read)


getWith2 :: String -> (a -> String) -> [a] -> a
getWith2 name f xs = case filter (\x -> f x == name) xs of
                        [x] -> x
                        [] -> error $ "getWith: Could not find " ++ name ++ " in " ++ strSet (map f xs)
                        _ -> error $ "getWith: Repetition of " ++ name ++ " in " ++ strSet (map f xs)


class QDatas a where
	qDatas :: a -> Datas



getData :: QDatas a => a -> DataName -> Data
getData q name = getWith2 name dataName (qDatas q)

getCtor :: QDatas a => a -> CtorName -> Ctor
getCtor q name = getWith2 name ctorName (concatMap ctors (qDatas q))


-- More complex, travel up the tree
getDataFromCtor :: QDatas a => a -> CtorName -> Data
getDataFromCtor q name = headNote ("q.Type.getDataFromCtor, asking for " ++ name)
    [d | d <- qDatas q, c <- ctors d, name == ctorName c]

getCtorFromArg :: QDatas a => a -> CtorArg -> Ctor
getCtorFromArg q name = headNote ("q.Type.getCtorFromArg, " ++ name)
    [c | d <- qDatas q, c <- ctors d, name `elem` ctorArgs c]


-- Compound, do common operations
getCtorsFromCtor :: QDatas a => a -> CtorName -> [CtorName]
getCtorsFromCtor q name = map ctorName $ ctors $ getDataFromCtor q name

getOtherCtors :: QDatas a => a -> CtorName -> [CtorName]
getOtherCtors q name = delete name (getCtorsFromCtor q name)


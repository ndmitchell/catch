
module Hite.DataType where

import Hite.TypeType
import General.General
import Data.List


-- THE DATA TYPES

type DataName = String
type CtorName = String
type CtorArg  = String


type Datas = [Data]

data Data = Data DataName [Ctor] [String]
            deriving (Show, Read)

data Ctor = Ctor CtorName [CtorArg] [TyType]
          deriving (Eq, Show, Read)


myAnswer :: String -> [a] -> a
myAnswer name [] = error $ "Could not find " ++ name
myAnswer name [x] = x
myAnswer name xs = error $ "Repetition of " ++ name


-- THE QUERY MECHANISMS

data AnswerDC = AnswerDC Data Ctor
data AnswerDCA = AnswerDCA Data Ctor (Int,CtorArg)

instance QData Data where rawData = id
instance QData AnswerDC where rawData (AnswerDC res _) = res
instance QCtor AnswerDC where rawCtor (AnswerDC _ res) = res
instance QData AnswerDCA where rawData (AnswerDCA res _ _) = res
instance QCtor AnswerDCA where rawCtor (AnswerDCA _ res _) = res
instance QCArg AnswerDCA where rawCArg (AnswerDCA _ _ res) = res


class QDatas a where
	rawDatas :: a -> Datas
	
class QData a where
	rawData :: a -> Data
	
class QData a => QCtor a where
	rawCtor :: a -> Ctor

class QCtor a => QCArg a where
	rawCArg :: a -> (Int,CtorArg)


-- THE SEARCH METHOD

getData :: QDatas a => a -> DataName -> Data
getData q name = myAnswer name [d | d@(Data nam _ _) <- rawDatas q, nam == name]

getCtor :: QDatas a => a -> CtorName -> AnswerDC
getCtor q name = myAnswer name [AnswerDC d c | d@(Data _ cs _) <- rawDatas q, c@(Ctor nam _ _) <- cs, nam == name]

getCArg :: QDatas a => a -> CtorArg -> AnswerDCA
getCArg q name = myAnswer name [AnswerDCA d c (i,a) | d@(Data _ cs _) <- rawDatas q, c@(Ctor _ as _) <- cs,
												      (i, a) <- zip [0..] as, a == name]

getCArgPos :: QDatas a => a -> CtorName -> Int -> AnswerDCA
getCArgPos q name pos = AnswerDCA d c (pos, as!!pos)
	where AnswerDC d c@(Ctor _ as _) = getCtor q name


-- THE QUERY METHODS

ctorNameRaw :: Ctor -> CtorName
ctorNameRaw (Ctor a _ _) = a

dataName :: QData a => a -> DataName
dataName q = let (Data res _ _) = rawData q in res

ctors :: QData a => a -> [AnswerDC]
ctors q = let d@(Data _ res _) = rawData q in map (AnswerDC d) res

frees :: QData a => a -> [String]
frees q = let (Data _ _ res) = rawData q in res


ctorName :: QCtor a => a -> CtorName
ctorName q = let (Ctor res _ _) = rawCtor q in res

ctorArgs :: QCtor a => a -> [CtorArg]
ctorArgs q = let (Ctor _ res _) = rawCtor q in res

types :: QCtor a => a -> [TyType]
types q = let (Ctor _ _ res) = rawCtor q in res


cargName :: QCArg a => a -> CtorArg
cargName = snd . rawCArg

cargPos :: QCArg a => a -> Int
cargPos = fst . rawCArg


-- COMPOUND

ctorNames :: QData a => a -> [CtorName]
ctorNames q = map ctorName $ ctors q


ctorOthers :: QCtor a => a -> [CtorName]
ctorOthers q = delete (ctorName q) (ctorNames q)

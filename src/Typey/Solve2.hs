
module Typey.Solve2(typeySolve2) where

import Typey.Type
import IO
import Hite


data Subtype2 = TCtor String
              | TFree String
              | TBind Subtype2 [Subtype2]
              | TArr Subtype2 Subtype2



typeySolve2 :: String -> Handle -> Hite -> DataM SmallT -> Func2M -> IO Bool
typeySolve2 file hndl hite datam funcm = return False







getSubtypes :: DataM SmallT -> Large2T -> [Subtype2]
getSubtypes datam (Free2T a) = [TFree a]
getSubtypes datam (Arr2T a b) = [TArr x y | x <- getSubtypes datam a, y <- getSubtypes datam b]

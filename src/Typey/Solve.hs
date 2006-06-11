
module Typey.Solve(typeySolve) where

import Hite
import Typey.Type
import Typey.Show
import Data.List


data Item = Item FuncName [Subtype] Subtype (Maybe Subtype)

instance Show Item where
    showList xs = showString $ unlines $ map show xs
    show (Item name args free solid) =
        name ++ " :: " ++
        (concat $ intersperse " -> " $ map show $ args ++ [free]) ++
        " = " ++ show solid
        
typeySolve :: Hite -> DataM SmallT -> FuncM -> [(FuncName, [Subtype])] -> [Subtype]
typeySolve hite datam funcm items = error $ "\n" ++ show (map f items)
    where
        f (name, arg) = Item name arg Top Nothing


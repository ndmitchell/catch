
module Typey.Solve(typeySolve) where

import Hite
import Typey.Type
import Typey.Show
import Data.List
import Data.Maybe


type State = (Hite, DataM SmallT, FuncM)


data Item = Item FuncName [Subtype] Subtype Pending

data Pending = Never
             | Later
             | Now Subtype
             deriving Show


instance Show Item where
    showList xs = showString $ "\n" ++ unlines (map show xs)
    show (Item name args free pending) =
        name ++ " :: " ++
        (concat $ intersperse " -> " $ map show $ args ++ [free]) ++
        " = " ++ show pending

        
-- return the subtypes of main that do not have bottom in
typeySolve :: Hite -> DataM SmallT -> FuncM -> Subtype
typeySolve hite datam funcm = error $ show expnd 
    where
        (n2,expnd) = expandRhs state n orig
        (n,orig) = addItems state [] pairings 0
        pairings = [(fname,args) | name <- funcs hite, let fname = funcName name, fname /= "error",
                                   args <- getSubtypesFunc datam $ fromJust $ lookup fname funcm]
        state = (hite,datam,funcm)


addItems :: State -> [Item] -> [(FuncName, [Subtype])] -> Int -> (Int, [Item])
addItems (hite,datam,funcm) items add norig = (n, items ++ i2)
    where
        (n,i2) = mapId f add norig
    
        f (name, arg) n = (n2, Item name arg r2 Later)
            where
                (FuncT _ _ res) = fromJust $ lookup name funcm
                (n2,r2) = getSubtypeFree datam res n


-- all rhs's must stop being Later
expandRhs :: State -> Int -> [Item] -> (Int, [Item])
expandRhs state n xs = (n,xs)
    where


        -- which functions would these things call
        --requestCallers :: Item -> [(FuncName, [Subtype])]
        --requestCallers


        -- instantiate, given you have now added the callers
        --useCallers





mapId :: (a -> Int -> (Int, b)) -> [a] -> Int -> (Int, [b])
mapId f [] n = (n,[])
mapId f (x:xs) n = (n3, x2:x3)
    where
        (n2,x2) = f x n
        (n3,x3) = mapId f xs n2



getSubtypeFree :: DataM SmallT -> LargeT -> Int -> (Int, Subtype)
getSubtypeFree datam (FreeL i) n = (n+1, SVar n)
getSubtypeFree datam (CtorL name xs) n
        | recursive = let (n1,a1,b1) = f n
                          (n2,a2,b2) = f n1
                      in (n2, Subtype a1 a2 b1 b2)
        | otherwise = let (n1,a1,b1) = f n
                      in (n1, Subtype a1 [] b1 [])
    where
        f n = (a,[UVar n],b)
            where (a,b) = mapId (getSubtypeFree datam) xs (n+1)
    
        (DataT free ctors) = fromJust $ lookup name datam
        recursive = any isRecursive ctors

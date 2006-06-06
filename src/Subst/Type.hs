
module Subst.Type where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap


data Env = Env Subst Bind

-- a variable is bound to a value
-- they must number 0..size-1
type Subst = IntMap.IntMap SExp

-- a function is bound to a variable
type Bind = Map.Map (String, [SExp]) Int

data SExp = SFree
          | SVar Int
          | SCtor String [SExp]
          | SFunc String [SExp]
          | SChoice [SExp]
          | SBot
          deriving (Eq, Ord)
          

allSExp :: SExp -> [SExp]
allSExp x = x : case x of
                SCtor _ xs -> concatMap allSExp xs
                SFunc _ xs -> concatMap allSExp xs
                SChoice xs -> concatMap allSExp xs
                _ -> []

mapSExp :: (SExp -> SExp) -> SExp -> SExp
mapSExp f x = f $ case x of
                 SCtor n xs -> SCtor n (fs xs)
                 SFunc n xs -> SFunc n (fs xs)
                 SChoice xs -> SChoice (fs xs)
                 _ -> x
    where
        fs = map (mapSExp f)


isConcrete :: SExp -> Bool
isConcrete SFree = True
isConcrete (SCtor _ xs) = all isConcrete xs
isConcrete _ = False

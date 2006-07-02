
module Typey.FuncTypes(funcTypes) where

import Typey.Type
import Typey.Subtype
import Typey.Permute


funcTypes :: DataM SmallT -> Func2M -> TypeList
funcTypes datam funcm = [(name, getFuncSubtypes datam typ) | (name, typ) <- funcm]

-- get all the possible subtypes of a function
getFuncSubtypes :: DataM SmallT -> Large2T -> TSubtype
getFuncSubtypes datam (Arr2T a b) = TFunc $ [TArr x (TFree []) | x <- permuteTypes datam a]
getFuncSubtypes datam x = TFree []


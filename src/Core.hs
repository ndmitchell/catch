
module Core(module Yhc.Core, module Core) where

import Yhc.Core


isCoreData (CoreData {}) = True
isCoreData _ = False

isCoreFunc = not . isCoreData


mergeCore :: Core -> Core -> Core
mergeCore (Core _ _ a) (Core _ _ b) = Core "" [] (ad ++ bd ++ af ++ bf)
    where
        (ad, af) = span isCoreData a
        (bd, bf) = span isCoreData b

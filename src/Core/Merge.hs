
module Core.Merge(mergeCore) where

import Core.Type


mergeCore :: Core -> Core -> Core
mergeCore (Core _ a) (Core _ b) = Core "" (ad ++ bd ++ af ++ bf)
    where
        (ad, af) = span isCoreData a
        (bd, bf) = span isCoreData b

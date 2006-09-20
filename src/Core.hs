
module Core(module Yhc.Core, module Core) where

import Yhc.Core


isCoreData (CoreData {}) = True
isCoreData _ = False

isCoreFunc = not . isCoreData


mergeCore :: Core -> Core -> Core
mergeCore (Core _ _ a1 b1) (Core _ _ a2 b2) = Core "" [] (a1++a2) (b1++b2)

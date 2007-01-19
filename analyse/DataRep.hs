
module DataRep where

import Yhc.Core


-- Given a constructor, get all the sibling constructors (including it)
ctorNames :: Core -> CoreCtorName -> [CoreCtorName]
ctorNames core = map coreCtorName . coreDataCtors . coreCtorData core


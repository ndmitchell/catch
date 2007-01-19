
module DataRep where

import Yhc.Core


ctorNames :: CoreData -> [CoreCtorName]
ctorNames = map coreCtorName . coreDataCtors


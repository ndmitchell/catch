
module DataRep where

import Yhc.Core


-- Given a constructor, get all the sibling constructors (including it)
ctorNames :: Core -> CoreCtorName -> [CoreCtorName]
ctorNames core = map coreCtorName . coreDataCtors . coreCtorData core


isFieldRecursive :: Core -> CoreFieldName -> Bool
isFieldRecursive core x = rec == typ
    where
        rec = unwords $ coreDataName dat : coreDataTypes dat
        typ = head [filter (`notElem` "()") a | (a,Just b) <- coreCtorFields $ coreFieldCtor core x, b == x]
        dat = coreFieldData core x



module Prepare.Library(createMain) where

import Yhc.Core
import Data.List


-- given the following non-partial exports
-- create a Main function if it doesn't already exist
createMain :: [String] -> Core -> Core
createMain exports core
        | "main" `elem` map coreFuncName (coreFuncs core) = core
        | otherwise = core{coreFuncs = newmain : newprim : newany : coreFuncs core}
    where
        newprim = CorePrim "main_prim" (length exports)
        newany  = CorePrim "main_any" 0
        newmain = CoreFunc "main" [] (CoreApp (CoreFun "main_prim") (map f exports))

        f name = coreApp (CoreFun n) (replicate args (CoreFun "main_any"))
            where
                n = coreName core ++ "." ++ name
                args = coreFuncArity $ coreFunc core n


module Hill.Simple(cmdsSimple) where

import Hill.Type


cmdsSimple =
    [hillCmdPure "use-error" (const useError)
    ]


---------------------------------------------------------------------

-- use the error command, if you can
useError :: Hill -> Hill
useError x = mapOverHill f x
    where
        f (Apply (Fun "error") [x]) = Error x
        f (Call "error" [x]) = Error x
        f x = x


---------------------------------------------------------------------

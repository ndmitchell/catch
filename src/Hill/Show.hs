
module Hill.Show where

import Hill.Type


instance Show ValueHill where
    show (ValueHill x) = show x
    show ValueNone = ""


instance Show Hill where
    show x = error "todo, Show Hill"

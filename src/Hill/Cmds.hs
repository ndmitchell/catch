
module Hill.Cmds(cmdsHill) where

import Hill.Type
import Hill.Reachable
import Hill.ShortName
import Hill.Simple


cmdsHill :: [HillAction]
cmdsHill = cmdsReachable ++ cmdsShortName ++ cmdsSimple

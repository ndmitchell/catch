
module Hill.Cmds(cmdsHill) where

import Hill.Type
import Hill.Reachable
import Hill.ShortName


cmdsHill :: [HillAction]
cmdsHill = cmdsReachable ++ cmdsShortName

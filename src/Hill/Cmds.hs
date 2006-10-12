
module Hill.Cmds(cmdsHill) where

import Hill.Type
import Hill.Reachable


cmdsHill :: [HillAction]
cmdsHill = cmdsReachable

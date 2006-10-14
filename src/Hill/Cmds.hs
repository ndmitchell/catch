
module Hill.Cmds(cmdsHill) where

import Hill.Type
import Hill.Reachable
import Hill.ShortName
import Hill.Simple
import Hill.ArityRaise
import Hill.Specialise
import Hill.Lambdas


cmdsHill :: [HillAction]
cmdsHill = cmdsReachable ++ cmdsShortName ++ cmdsSimple ++ cmdsArityRaise ++ cmdsSpecialise ++ cmdsLambda

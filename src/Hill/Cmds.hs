
module Hill.Cmds(cmdsHill) where

import Hill.Type
import Hill.Reachable
import Hill.ShortName
import Hill.Simple
import Hill.ArityRaise
import Hill.Specialise
import Hill.Lambdas
import Hill.Lets
import Hill.Partial
import Hill.Defunc


cmdsHill :: [HillAction]
cmdsHill = cmdsReachable ++ cmdsShortName ++ cmdsSimple ++ cmdsArityRaise ++
           cmdsSpecialise ++ cmdsLambda ++ cmdsLets ++ cmdsPartial ++ cmdsDefunc

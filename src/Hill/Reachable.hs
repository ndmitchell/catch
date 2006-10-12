
module Hill.Reachable(reachable, cmdsReachable) where

import Hill.Type


cmdsReachable = [hillCmdPure "reachable" reachable]



reachable :: FuncName -> Hill -> Hill
reachable _ x = x

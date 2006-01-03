
module Hite
    (
    module Hite.Type,
    module Hite.Read, module Hite.Show, module Hite.Eq,
    module Hite.Forward, module Hite.Check, module Hite.Inline, module Hite.Reachable,
    module Hite.Firstify,
    cmdLine
    )
    where

import Hite.Type
import Hite.Read
import Hite.Show
import Hite.Eq
import Hite.Forward
import Hite.Check
import Hite.Inline
import Hite.Reachable
import Hite.Firstify


import CmdLine


cmdLine = [
            CmdLine "hite" OptString OptHite (const return) "Reads in a hite file",
            f "inline" inline "Inline some definitions",
            f "forward" forward "Perform forward motion on some definitions",
            f "check" (\x -> if check x then x else undefined) "Check some hite is valid",
            g "reachable" (defMain reachable) "Do reachable analysis",
            f "firstify" firstify "Perform firstification"
        ]
    where
        f a b c = g a (const b) c
        g a b c = CmdLine a OptHite OptHite (\a (DatHite x) -> return $ DatHite (b a x)) c
        
        defMain f "" = f "main"
        defMain f x  = f x

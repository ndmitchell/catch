
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
            CmdLine "hite" OptString OptHite return "Reads in a hite file",
            f "inline" inline "Inline some definitions",
            f "forward" forward "Perform forward motion on some definitions"
        ]
    where
        f a b c = CmdLine a OptHite OptHite (\(DatHite x) -> return $ DatHite (b x)) c

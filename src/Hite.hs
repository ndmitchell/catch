
module Hite
    (
    cmds,
    module Hite.Type,
    module Hite.Read, module Hite.Show, module Hite.Eq,
    
    module Hite.Blur
    )
    where


import General.Commands

import Hite.Type
import Hite.Read
import Hite.Show
import Hite.Eq

import Hite.Check
import Hite.Inline
import Hite.Reachable
import Hite.ErrorFail
import Hite.ShortName
import Hite.Blur
import Hite.Normalise
import Hite.Defunc
import Hite.CaseLift
import Hite.ArityRaise
import Hite.Consts
import Hite.MakeMove


cmds :: [Command Hite]
cmds = [Hite.Reachable.cmd, Hite.ShortName.cmd, Hite.ErrorFail.cmd, Hite.CaseLift.cmd,
        Hite.ArityRaise.cmd, Hite.Consts.cmd, Hite.MakeMove.cmd, Hite.Inline.cmd,
        Hite.Defunc.cmd]

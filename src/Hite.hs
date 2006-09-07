
module Hite
    (
    cmds,
    module Hite.Type,
    module Hite.Show, module Hite.Eq,
    
    module Hite.Blur, module Hite.Cache,
    module Hite.Fringe,
    
    shortName, reachable
    )
    where


import General.Commands

import Hite.Type
import Hite.Show
import Hite.Eq
import Hite.Cache

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
import Hite.MCase
import Hite.Fringe
import Hite.OneCall
import Hite.ConstLift
import Hite.ErrorCall
import Hite.ArgElim
import Hite.CaseExpand
import Hite.StringExpand
import Hite.CtorCollapse
import Hite.Defunc2
import Hite.Dedict
import Hite.Transform
import Hite.Primitive


cmds :: [Command Hite]
cmds = [Hite.Reachable.cmd, Hite.ShortName.cmd, Hite.ErrorFail.cmd, Hite.CaseLift.cmd,
        Hite.ArityRaise.cmd, Hite.Consts.cmd, Hite.MakeMove.cmd, Hite.Inline.cmd,
        Hite.Defunc.cmd, Hite.MCase.cmd, Hite.Check.cmd, Hite.OneCall.cmd,
        Hite.ConstLift.cmd, Hite.ErrorCall.cmd, Hite.ArgElim.cmd, Hite.CaseExpand.cmd,
        Hite.StringExpand.cmd, Hite.CtorCollapse.cmd, Hite.Defunc2.cmd, Hite.Dedict.cmd,
        Hite.Transform.cmd, Hite.Primitive.cmd]

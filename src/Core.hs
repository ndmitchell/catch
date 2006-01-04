
module Core
    (
    module Core.Type,
    module Core.Read, module Core.Show,
    module Core.Reduce,
    cmdLine
    )
    where

import Core.Type
import Core.Read
import Core.Show
import Core.Reduce


import CmdLineData


cmdLine = [
            CmdLine "core" OptCore OptCore (const return) "Reads in a core file"
          ]

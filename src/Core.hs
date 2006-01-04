
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
            CmdLine "core" OptCore OptCore (const return) "Reads in a core file",
            CmdLine "cores" OptInputs OptCore (const f) "Reads in many core files"
          ]
    where
        f (DatInputs xs) =
            do
                srcs <- mapM readFile xs
                let inps = concatMap ((\(Core a) -> a) . readCore) srcs
                return $ DatCore $ Core inps

                               

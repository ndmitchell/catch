
module Actions(cmdLine) where

import CmdLineData

import Hite
import Core
import Convert.CoreHite



cmdLine = [
            CmdLine "core" OptCore OptCore (const return) "Reads in a core file",
            CmdLine "cores" OptInputs OptCore (const readCores) "Reads in many core files",
            f "hite" id "Reads in a hite file",
            f "inline" inline "Inline some definitions",
            f "errorfail" errorFail "Remove failure type errors",
            f "forward" forward "Perform forward motion on some definitions",
            f "kind" kind "Add kind annotations",
            f "specialise" specialise "Perform specialisation",
            f "check" (\x -> if check x then x else undefined) "Check some hite is valid",
            g "reachable" (defMain reachable) "Do reachable analysis",
            f "firstify" firstify "Perform firstification",
            CmdLine "merge" OptHite OptHite mergeHite "Merge in some hite code",
            CmdLine "data" OptHite OptHite mergeData "Merge in some data definitions",
            CmdLine "preamble" OptCore OptHite (const preamble) "Add preamble definitions"
        ]
    where
        f a b c = g a (const b) c
        g a b c = CmdLine a OptHite OptHite (\a (DatHite x) -> return $ DatHite (b a x)) c
        
        defMain f "" = f "main"
        defMain f x  = f x


readCores (DatInputs xs) =
    do
        srcs <- mapM readFile xs
        let inps = concatMap ((\(Core a) -> a) . readCore) srcs
        return $ DatCore $ Core inps


mergeHite file (DatHite (Hite d f)) = 
    do
        src <- readFile file
        let Hite d2 f2 = read src
        return $ DatHite $ Hite (d2 ++ d) (f2 ++ f)


mergeData file (DatHite (Hite d f)) = 
    do
        src <- readFile file
        return $ DatHite $ fixData $ Hite (readData src ++ d) f


preamble (DatCore (Core x)) =
    do
        src <- readFile "preamble.core"
        let Core y = readCore src
            hite = coreHite $ Core (y ++ x)
        mergeData "preamble.data" (DatHite hite)

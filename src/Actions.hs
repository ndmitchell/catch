
module Actions(cmdLine) where

import CmdLineData

import Hite
import Core
import Convert.CoreHite
import Checker.CaseCheck



cmdLine = [
            CmdLine "core" OptCore OptCore (const return) "Reads in a core file",
            CmdLine "cores" OptInputs OptCore (const readCores) "Reads in many core files",
            f "hite" id "Reads in a hite file",
            f "inline" inline "Inline some definitions",
            f "errorfail" errorFail "Remove failure type errors",
            f "forward" forward "Perform forward motion on some definitions",
            f "short" shortName "Make some names shorter",
            f "kind" kind "Add kind annotations",
            f "dead" deadArgs "Remove dead arguments",
            f "specialise" specialise "Perform specialisation",
            f "check" (\x -> if check x then x else undefined) "Check some hite is valid",
            g "reachable" (defMain reachable) "Do reachable analysis",
            f "firstify" firstify "Perform firstification",
            f "defunc" defunc "Perform defunctionalisation",
            CmdLine "merge" OptHite OptHite mergeHite "Merge in some hite code",
            CmdLine "data" OptHite OptHite mergeData "Merge in some data definitions",
            CmdLine "preamble" OptCore OptCore (const preamble) "Add preamble definitions",
            CmdLine "prepare" OptCore OptHite (const prepare) "Prepare a file for checking",
            CmdLine "example" OptInputs OptHite (const example) "Read in an example",
            CmdLine "active" OptInputs OptHite (const active) "Perform standard transformations",
            CmdLine "case" OptHite OptAction (const runCaseCheck) "Case check a file"
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
        src <- readFile "Preamble/Preamble.core"
        let Core y = readCore src
        return $ DatCore $ Core (y ++ x)


prepare x =
    do 
        DatCore a <- preamble x
        return $
                wrap kind $
                wrap (reachable "main") $
                wrap inline $
                wrap kind $
                wrap specialise $
                wrap errorFail $
                wrap shortName $
                DatHite (coreHite a)
    where
        wrap f (DatHite x) = DatHite (f x)


runCaseCheck (DatHite h) =
    do
        caseCheck h
        return (DatAction "")


example (DatInputs [x]) = 
    do src <- readFile $ "Example/" ++ x ++ ".hs.core"
       prepare (DatCore (readCore src))


active (DatInputs [x]) = 
    do src <- readFile $ "Example/" ++ x ++ ".hs.core"
       DatCore a <- preamble (DatCore (readCore src))
       return $
                wrap (reachable "main") $
                wrap defunc $
                wrap (reachable "main") $
                wrap inline $ 
                wrap (reachable "main") $
                wrap errorFail $
                wrap shortName $
                DatHite (coreHite a)
    where
        wrap f (DatHite x) = DatHite (f x)


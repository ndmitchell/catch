
module CmdLineData where


import Hite.Type
import Core.Type
import Core.Show


data CmdOpt = OptString | OptHite | OptCore | OptAction | OptInputs | OptSpecial Int
              deriving Eq


instance Show CmdOpt where
    show OptString = "string"
    show OptHite = "Hite"
    show OptCore = "Core"
    show OptAction = "()"
    show OptInputs = "*"


data CmdDat = DatString String | DatHite Hite | DatCore Core | DatAction String | DatInputs [String]

datToOpt (DatString _) = OptString
datToOpt (DatHite   _) = OptHite
datToOpt (DatCore   _) = OptCore
datToOpt (DatAction _) = OptAction
datToOpt (DatInputs _) = OptInputs



data CmdLine = CmdLine String CmdOpt CmdOpt (String -> CmdDat -> IO CmdDat) String


instance Show CmdLine where
    show c = cmdLineName c


instance Show CmdDat where
    show (DatString x) = x
    show (DatHite x) = show x
    show (DatCore x) = showCore x
    show (DatAction x) = x


cmdLineName (CmdLine a _ _ _ _) = a



module CmdLineData where


import Hite.Type


data CmdOpt = OptString | OptHite | OptAction | OptInputs | OptSpecial Int
              deriving Eq


instance Show CmdOpt where
    show OptString = "string"
    show OptHite = "Hite"
    show OptAction = "()"
    show OptInputs = "*"


data CmdDat = DatString String | DatHite Hite | DatAction String | DatInputs [String]

datToOpt (DatString _) = OptString
datToOpt (DatHite   _) = OptHite
datToOpt (DatAction _) = OptAction
datToOpt (DatInputs _) = OptInputs



data CmdLine = CmdLine String CmdOpt CmdOpt (String -> CmdDat -> IO CmdDat) String


instance Show CmdLine where
    show c = cmdLineName c


instance Show CmdDat where
    show (DatString x) = x
    show (DatHite x) = show x
    show (DatAction x) = x


cmdLineName (CmdLine a _ _ _ _) = a


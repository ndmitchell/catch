
module Backend.Backend(backend) where

import System.IO
import Hite


backend :: String -> Handle -> Hite -> IO Bool
backend file hndl hite = do
        writeFile ("Logs/" ++ file ++ ".c") (convHite hite)
        return True


convHite :: Hite -> String
convHite hite = "#include \"Backend.h\"\n\nvoid main()\n{\n\t" ++ escFunc "main" ++ "();\n}\n\n" ++
                concatMap (convFunc hite) (funcs hite)


escFunc x = "ESC_" ++ x


convFunc :: Hite -> Func -> String
convFunc hite func@(Func name args body _) = 
    "void " ++ escFunc name ++ "()\n{\n" ++
    "\tint* vars = stack_top();\n" ++
    unlines (indent $ convExpr hite func body) ++
    "\tstack_pop();\n" ++
    "}\n\n"


convExpr :: Hite -> Func -> Expr -> [String]
convExpr hite func expr = ["todo!"]


indent :: [String] -> [String]
indent = map ('\t':)

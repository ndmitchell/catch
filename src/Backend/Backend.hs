
module Backend.Backend(backend) where

import System.IO
import Data.Char
import Data.List
import General.General hiding (indent)
import Hite
import Hite.Normalise


backend :: String -> Handle -> Hite -> IO Bool
backend file hndl hite = do
        writeFile ("Logs/" ++ file ++ ".c") (convHite hite)
        return True


convHite :: Hite -> String
convHite oite = concatMap convData (datas hite) ++
                "\n\n#include \"Backend.h\"\n\n" ++
                unlines (map preDecl $ funcs hite) ++
                "\n\nint main()\n{\n\tstack_push(0);\n\t" ++ escFunc "main" ++ "();\n\treturn 0;\n}\n\n" ++
                concatMap (convFunc hite) (funcs hite)
    where
        hite = mapExpr f oite
        
        f (Prim name xs) = Call (CallFunc ("prim_" ++ name)) xs
        f x = x
        
        preDecl x = "int* " ++ escFunc (funcName x) ++ "();"


convData :: Data -> String
convData (Data nam xs _) 
        | nam == "Char" = ""
        | otherwise = unlines $ zipWith f [0..] xs
    where
        f n (Ctor nam _ _) = "#define " ++ escCtor nam ++ " " ++ show n



escFunc = escAny "func"

escCtor x | "Char_" `isPrefixOf` x = show $ fromCharCtor x
escCtor x = escAny "ctor" x

escAny prefix x = prefix ++ "_" ++ concatMap f x
    where
        f x | isAlphaNum x = [x]
            | otherwise = '_' : show (ord x)


convFunc :: Hite -> Func -> String
convFunc hite func@(Func name args body _) = 
    "int* " ++ escFunc name ++ "()\n{\n" ++
    "\tint* vars = stack_top();\n" ++
    "\tint* res;\n" ++
    unlines (indent $ convExpr hite func body) ++
    "}\n\n"


convExpr :: Hite -> Func -> Expr -> [String]
convExpr hite func expr = demand expr
    where
        argToInt :: FuncArg -> String
        argToInt x = show $ lookupJust x $ zip (funcArgs func) [2..]
        
        cargToInt :: CtorArg -> String
        cargToInt x = show $ cargPos (getCArg hite x) + 1

        
        demand (Case x xs) =
            ["switch (eval(" ++ alloc x ++ "))"
            ,"{"]
            ++ indent (concatMap f xs) ++
            ["};"]
            where
                f (alt,expr) = g alt : "{" : indent (demand expr) ++ ["}","break;"]
                g "" = "default:"
                g alt = "case " ++ escCtor alt ++ ":"

        demand (Error xs) = ["error(" ++ show xs ++ ");"]
    
        demand (Call (CallFunc name) args) =
            ["stack_push(allocCall" ++ nargs ++ "(" ++ ename ++ concatMap ((',':) . alloc) args ++ "));"
            ,"res = " ++ escFunc name ++ "();"
            ,"stack_pop();"
            ,"return res;"
            ]
            where
                ename = escFunc name
                nargs = show $ length args

        demand x = ["res = " ++ alloc x ++ ";"
                   ,"eval(res);"
                   ,"return res;"]
        
        alloc (Make x xs) =
            "allocCtor" ++ show (length xs) ++ "(" ++ escCtor x ++
            concatMap ((',':) . alloc) xs ++ ")"

        alloc (Call (CallFunc x) xs) =
            "allocCall" ++ show (length xs) ++ "(" ++ escFunc x ++
            concatMap ((',':) . alloc) xs ++ ")"
        
        alloc (Var x) = "((int*) vars[" ++ argToInt x ++ "])";
        
        alloc (Sel x y) | False && isVarSels x = "(" ++ alloc x ++ ")[" ++ s ++ "]"
                        | otherwise = "allocCall1(follow" ++ s ++ ", " ++ alloc x ++ ")"
            where s = cargToInt y
        
        alloc x = "todo"


        isVarSels (Sel x y) = isVarSels x
        isVarSels (Var x) = True
        isVarSels _ = False


indent :: [String] -> [String]
indent = map ('\t':)

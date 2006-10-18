
module Hill.Haskell(cmdsHaskell) where

import Hill.Type
import Hill.PrimOp
import Front.CmdLine
import Data.List
import Data.Char
import General.General


cmdsHaskell = [Action "hill-haskell" outputHaskell]


---------------------------------------------------------------------

-- make it suitable for Haskell
makeHaskell :: Hill -> Hill
makeHaskell hill = hill {- completeCase $ caseBeforeSel hill





caseBeforeSel :: Hill -> Hill
caseBeforeSel hill = hill{funcs = [func{body = f [] (body func)} | func <- funcs res]}
    where
        res = mapOverHill addCase hill
        
        f :: [Expr] -> Expr -> Expr
        f seen (Sel 
        
        
        addCase x = allExpr x
-}



outputHaskell :: CmdLineState -> String -> ValueHill -> IO ValueHill
outputHaskell state _ (ValueHill badhill) = do
        let file = cmdLineOutput state "hs"
        writeFile file (unlines outHill)
        return $ ValueHill hill
    where
        hill = makeHaskell badhill
    
        outHill = "module Main(main) where" : 
                  map ("import "++) primImports ++
                  ("main = unio (" ++ oc "main" ++ ")") :
                  ("io x = " ++ od "IO" ++ " $! unsafePerformIO x") :
                  ("bool x = if x then " ++ od "True" ++ " else " ++ od "False") :
                  ("unio (" ++ od "IO" ++ " x) = (return :: a -> IO a) x") :
                  map outPrim primitives ++
                  map outData (datas hill) ++
                  map outFunc (funcs hill)
    
        primitives = snub [x | Prim x _ <- allOverHill hill]
    
        outData (Data name ctrs typs) =
                "data " ++ od name ++ concatMap (' ':) typs ++ " = "
                        ++ concat (intersperse " | " $ map outCtor ctrs)
                
            where
                outCtor (Ctor name _ typ) = od name ++ concatMap ((' ':) . outType) typ
                
                outType (TyFree x) = x
                outType (TyCon x xs) = "(" ++ od x ++ concatMap ((' ':) . outType) xs ++ ")"
                
                {-
            
                outCtor (name, n, i) = od name ++ concatMap (' ':) ["t" ++ show n ++ "_" ++ show j | j <- [1..i]]
                tvars = ["t" ++ show n ++ "_" ++ show j | (_, n, i) <- frees, j <- [1..i]]
                frees = [(nam, n, length x, typ) | (n, Ctor nam x typ) <- zip [1..] ctrs]
                -}


        outFunc (Func name args body) = oc name ++ concat [' ':'v':show i | i <- args] ++ " = " ++ outExpr body
        
        
        outExpr (Let binds x) = "(let {" ++ intercat " ; " ['v':show a ++ " = " ++ outExpr b | (a,b) <- binds] ++ "} in " ++ outExpr x ++ ")"
        outExpr (Var x) = "v" ++ show x
        outExpr (Sel x path) = outExpr x ++ "_" ++ show (p+1)
            where p = cargPos (getCArg hill path)
        outExpr (Call x xs) = "(" ++ oc x ++ concatMap ((' ':) . outExpr) xs ++ ")"
        outExpr (Prim x xs) = outExpr (Call x xs)
        outExpr (Make x xs) = "(" ++ od x ++ concatMap ((' ':) . outExpr) xs ++ ")"
        outExpr (Const x) = outConst x
        outExpr (Case on alts) = "(case " ++ outExpr on ++ " of {" ++ intercat " ; " (map f alts) ++ "})"
            where
                f x = g x ++ " -> " ++ outExpr (altExpr x)
            
                g (Default x) = "_"
                g (AltCtr x y) | isVarSel on = od x ++ concat [' ':ons++"_"++show i | i <- [1..arity]]
                               | otherwise = od x ++ concat (replicate arity " _")
                    where
                        ons = outExpr on
                        arity = length $ ctorArgs $ getCtor hill x
                
                g (AltConst x y) = outConst x
        
        outExpr x = error $ show x
        
        
        outConst (AInt x) = "(" ++ show x ++ " :: Int)"
        outConst (AChar x) = outConst (AInt (ord x))
        outConst (AString []) = od "[]"
        outConst (AString (x:xs)) = "(" ++ od ":" ++ " " ++ outConst (AChar x) ++ outConst (AString xs) ++ ")"
        
        
        outPrim name = oc name ++ " = " ++ primHaskell name

        od x = "D" ++ out x
        oc x = "f" ++ out x
        
        out xs = concatMap f xs
            where
                f x | isAlphaNum x = [x]
                    | otherwise = '_' : show (ord x)


module Hill.Haskell(cmdsHaskell) where

import Hill.Type
import Hill.PrimOp
import Hill.Show
import Front.CmdLine
import Data.List
import Data.Char
import General.General


cmdsHaskell = [Action "hill-haskell" outputHaskell]


---------------------------------------------------------------------

-- make it suitable for Haskell
makeHaskell :: Hill -> Hill
makeHaskell hill = mapOverHill reqLets $ mapOverHill letDownwards hill
    where
        letDownwards (Let binds (Case on alts)) = mkLet keep (Case on alts2)
            where
                freeze = snub [i | Var i <- allOverHill on]
                (keep,move) = partition ((`elem` freeze) . fst) binds
                
                alts2 = [alt{altExpr = letDownwards $ mkLet move (altExpr alt)} | alt <- alts]
        letDownwards x = x            


        reqLets (Let binds x) = mkLet binds2 x
            where
                req = requiredFree x
                binds2 = filter ((`elem` req) . fst) binds
        reqLets x = x


outputHaskell :: CmdLineState -> String -> ValueHill -> IO ValueHill
outputHaskell state _ (ValueHill badhill) = do
        let file = cmdLineOutput state "hs"
        writeFile file (unlines outHill)
        return $ ValueHill hill
    where
        hill = makeHaskell badhill
        
        keepData = ["IO","Bool","[]"]
        keepCtor = ["True","False",":","[]"]
    
        outHill = "module Main(main) where" : 
                  map ("import "++) primImports ++
                  ("main" ++ mainargs ++ " = unio (" ++ oc "main" ++ mainargs ++ ")") :
                  ("io x = " ++ od "IO" ++ " $! unsafePerformIO x") :
                  ("unio (" ++ od "IO" ++ " x) = x") : -- (return :: a -> IO a) x") :
                  ("err x = error (map chr x)") :
                  ("data " ++ od "IO" ++ " x = " ++ od "IO" ++ " x") :
                  map outPrim primitives ++
                  concatMap outData (datas hill) ++
                  map outFunc (funcs hill)
    
        mainargs = concat [' ':'v':show i | i <- [0..(length $ funcArgs $ getFunc hill "main")-1]]
    
        primitives = snub [x | Prim x _ <- allOverHill hill]
    
        outData (Data name ctrs typs)
                | name `elem` keepData = []
                | otherwise = 
                ["data " ++ od name ++ concatMap (' ':) typs ++ " = "
                         ++ concat (intersperse " | " $ map outCtor ctrs)]
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
        outExpr (Error x) = "(err " ++ outExpr x ++ ")"
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
        
        outExpr x = error $ "outExpr, unhandled " ++ show x
        
        
        outConst (AInt x) = "(" ++ show x ++ " :: Int)"
        outConst (AChar x) = outConst (AInt (ord x))
        outConst (AInteger x) = "(" ++ show x ++ " :: Integer)"
        outConst (AString []) = od "[]"
        outConst (AString (x:xs)) = "(" ++ od ":" ++ " " ++ outConst (AChar x) ++ outConst (AString xs) ++ ")"
        outConst x = error $ "outConst, unhandled " ++ show x
        
        
        outPrim name = oc name ++ " = " ++ primHaskell name

        od x | x `elem` keepCtor = "(" ++ x ++ ")"
             | otherwise = "D" ++ out x
        oc x = "f" ++ out x
        
        out xs = concatMap f xs
            where
                f x | isAlphaNum x = [x]
                    | otherwise = '_' : show (ord x)

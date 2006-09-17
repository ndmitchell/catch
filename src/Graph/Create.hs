
module Graph.Create(createGraph) where

import Graph.Type
import Graph.Show
import Graph.Rename

import Hite
import General.General
import Data.List
import Data.Maybe
import Data.Predicate


createGraph :: Hite -> Graph
createGraph hite@(Hite _ funcs) = nodes
    where
        nodes = map baseNode table ++ map linkNode links
        links = zip [length funcs..] $ concatMap f $ filter (\x -> funcName x /= "error") funcs
        table = ("main",0) : zip (map funcName funcs \\ ["main"]) [1..]
        
        baseNode ("error",_) = Node "error" [] [GraphEnd]
        baseNode (name,_) = Node name [n | (n,(from,to,with)) <- links, from == name] []
        linkNode (_,(from,to,with)) = Node "" [fromJust $ lookup to table] [with]
        
        
        f :: Func -> [(FuncName, FuncName, Rewrite)]
        f func@(Func name _ (MCase alts) _) = concatMap g alts
            where
                g (MCaseAlt p ex) = [(name,to, Rewrite l2 r2) | (to,r) <- rhs ex, let (l2,r2) = lhs func r p]
        
        lhs :: Func -> GExp -> Pred MCaseOpt -> (GExp, GExp)
        lhs func right p = (applyRename ren base, applyRename ren right)
            where
                ren = getRename hite func base p
                base = GCtor "." $ map GVar (funcArgs func)
                
                ps = fromAnd p
                reps = map (generate . fromLit) ps
                
                -- generate an initial expression, with its name
                generate :: MCaseOpt -> (String, GExp)
                generate (MCaseOpt s ctor) = (var,
                        GCtor ctor [GVar $ var ++ "." ++ x | x <- ctorArgs $ getCtor hite ctor])
                    where var = output s
                
                solve :: GExp -> [(String, GExp)] -> GExp
                solve b [] = b
                solve b (x:xs) = solve (merge x b) $ map (\(a,b) -> (a,merge x b)) xs
                
                merge :: (String, GExp) -> GExp -> GExp
                merge (name,rep) x = mapGExp f x
                    where
                        f (GVar n) | n == name = rep
                        f x = x

        rhs :: Expr -> [(FuncName, GExp)]
        rhs x = [(name, GCtor "." (map trans args)) | Call (CallFunc name) args <- allExpr x]
        
        trans :: Expr -> GExp
        trans x = case x of
                      Call (CallFunc name) xs -> GFunc name (GCtor "." $ map trans xs)
                      Make name xs -> GCtor name (map trans xs)
                      Var name -> GVar name
                      Sel on path -> let GVar x = trans on
                                     in  GVar (x ++ "." ++ path)
                      Msg s -> GStr s

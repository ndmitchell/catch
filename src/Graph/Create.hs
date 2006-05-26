
module Graph.Create(createGraph) where

import Graph.Type
import Graph.Show
import Hite
import List
import Maybe
import Data.Predicate


createGraph :: Hite -> Graph
createGraph (Hite _ funcs) = Graph nodes
    where
        nodes = map baseNode table ++ map linkNode links
        links = zip [length funcs..] $ concatMap f $ filter (\x -> funcName x /= "error") funcs
        table = ("main",0) : zip (map funcName funcs \\ ["main"]) [1..]
        
        baseNode ("error",_) = Node "error" [] (Just GraphEnd)
        baseNode (name,_) = Node name [n | (n,(from,to,with)) <- links, from == name] Nothing
        linkNode (_,(from,to,with)) = Node "" [fromJust $ lookup to table] (Just with)
        
        
        f :: Func -> [(FuncName, FuncName, Rewrite)]
        f (Func name args (MCase alts) _) = concatMap g alts
            where
                g (MCaseAlt p ex) = [(name,to, Rewrite (lhs args p) r) | (to,r) <- rhs ex]
        
        lhs :: [FuncArg] -> Pred MCaseOpt -> GExp
        lhs args p | isTrue p = GCtor "." $ map GVar args
                   | isLit p = let MCaseOpt (Var var) cond = fromLit p
                               in GCtor "." [GCtor cond []]
                   | otherwise = error $ "lhs: " ++ show p
        
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

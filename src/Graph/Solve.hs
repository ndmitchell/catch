
module Graph.Solve(solveGraph) where

import Graph.Type
import Graph.Draw
import Hite
import Data.Maybe


solveGraph :: Hite -> Graph -> IO Bool
solveGraph hite graph = do drawGraph graph2 "Temp-Graph"
                           return False
    where
        graph2 = controlReduce $ reachFailure $ removeSimpleRewrite graph




-- if a rewrite changes nothing, and has not ctor's apart from the base "."
-- then it is redundant
removeSimpleRewrite :: Graph -> Graph
removeSimpleRewrite nodes = map f nodes
    where
        f (n@Node{rewrite=Just r}) | isSimple r = n{rewrite=Nothing}
        f x = x
        
        isSimple (Rewrite (GCtor "." a) (GCtor "." b)) = all isGVar (a ++ b)
        isSimple _ = False



-- Remove all control->control nodes
controlReduce :: Graph -> Graph
controlReduce graph = if length graph == length newgraph then graph else controlReduce newgraph
    where
        newgraph = controlReduction graph


controlReduction :: Graph -> Graph
controlReduction graph = gc $ map change graph
    where
        redundant = map fst $ filter isRedundant $ zip [0..] graph
        
        isRedundant (num, node) = isNothing (rewrite node) && all f (edges node)
            where
                f n = {- isNothing (rewrite (nodes !! n)) && -} not (num `elem` gReachable graph n)

        change node = node{edges = concatMap newedge (edges node)}
        newedge n | n `elem` redundant = edges (graph !! n)
                  | otherwise = [n]


reachFailure :: Graph -> Graph
reachFailure graph = gc $ map change graph
    where
        safe = map fst $ filter isSafe $ zip [0..] graph
        
        isSafe (num, node) = not $ any isGraphEnd $ catMaybes [rewrite (graph !! n) | n <- gReachable graph num]

        change node = node{edges = filter (\x -> not (x `elem` safe)) (edges node)}


module Graph.Solve(solveGraph) where

import Graph.Type
import Graph.Draw
import Hite
import Data.Maybe
import Data.List
import Debug.Trace


solveGraph :: Hite -> Graph -> IO Bool
solveGraph hite graph = do drawGraph graph2 "Temp-Graph"
                           return False
    where
        graph2 = graphControlDelete $ graphItemDelete graph




-- * Item removal
-- Delete all redundant bits

graphItemDelete :: Graph -> Graph
graphItemDelete = removeSimpleRewrite . reachFailure

-- if a rewrite changes nothing, and has not ctor's apart from the base "."
-- then it is redundant
removeSimpleRewrite :: Graph -> Graph
removeSimpleRewrite nodes = map f nodes
    where
        f (n@Node{rewrite=[r]}) | isSimple r = n{rewrite=[]}
        f x = x
        
        isSimple (Rewrite (GCtor "." a) (GCtor "." b)) = all isGVar (a ++ b)
        isSimple _ = False


reachFailure :: Graph -> Graph
reachFailure graph = gc $ map change graph
    where
        safe = map fst $ filter isSafe $ zip [0..] graph
        
        isSafe (num, node) = not $ any isGraphEnd $ concat [rewrite (graph !! n) | n <- gReachable graph num]

        change node = node{edges = filter (\x -> not (x `elem` safe)) (edges node)}


-- * Control Reduction
-- Remove all redundant control loops, just compress things

graphControlDelete :: Graph -> Graph
graphControlDelete = yPromotion . labelInline . controlReduce


-- if a node is at the end, then promote it to be the contents of all its inners
yPromotion :: Graph -> Graph
yPromotion graph = if null canPromote then graph else yPromotion $ gc $ change (length graph) graph
    where
        canPromote = [n | (n,x) <- labeled graph, let dest = gReachables graph (edges x),
                          not (n `elem` dest), let income = incoming graph n \\ dest,
                          length income >= 2]

        (promote, promnode) = (head canPromote, graph !! promote)
        
        change n [] = []
        change n (node:nodes) | promote `elem` es = node{edges = es2} : change (n+1) (nodes ++ [promnode])
                              | otherwise = node : change n nodes
            where
                es = edges node
                es2 = map (\x -> if x == promote then n else x) es


-- if the root node (0) is a single pointer onwards, compress it
rootPromote :: Graph -> Graph
rootPromote graph = gc $ f 0 : tail graph
    where
        f n | length (edges nn) == 1 && null (rewrite nn) = f (head $ edges nn)
            | otherwise = nn
            where nn = graph !! n


labelInline :: Graph -> Graph
labelInline graph = gc $ map (f []) (labels graph)
    where
        f done x | x `elem` done = {- trace ("labelInline1: " ++ show (done,x)) $ -} Node "" [x] []
                 | null (edges node) || length (edges node) > 1 = {- trace ("labelInline2: " ++ show (done,x)) -} node
                 | otherwise = let Node nam edg rw = f (x:done) (head $ edges node)
                               in Node nam edg (rewrite node ++ rw)
            where
                node = graph !! x
        


-- Remove all control->control nodes
controlReduce :: Graph -> Graph
controlReduce graph = if length graph == length newgraph then graph else controlReduce newgraph
    where
        newgraph = controlReduction graph


controlReduction :: Graph -> Graph
controlReduction graph = gc $ map change graph
    where
        redundant = map fst $ filter isRedundant $ zip [0..] graph
        
        isRedundant (num, node) = null (rewrite node) && all f (edges node)
            where
                f n = not (num `elem` gReachable graph n)

        change node = node{edges = concatMap newedge (edges node)}
        newedge n | n `elem` redundant = edges (graph !! n)
                  | otherwise = [n]



module Graph.Draw(drawGraph) where

import Graph.Type
import Graph.Show
import System


drawGraph :: Graph -> FilePath -> IO ()
drawGraph graph file = do writeFile (file ++ ".dot") (showGraph graph)
                          system $ "dot " ++ file ++ ".dot -Tsvg -Grankdir=LR -Gdpi=70.0 > " ++ file ++ ".svg"
                          return ()


showGraph :: Graph -> String
showGraph xs = unlines $ ["Digraph catch{"] ++ concat (zipWith f [0..] xs) ++ ["}"]
    where
        f n (Node name edges rewrite) =
              ["n" ++ show n ++ "0[label=\"" ++ show n ++ "," ++ name ++ "\",color=\"green\"]"] ++
              showRewrite ("n" ++ show n) 0 rewrite edges
              

showRewrite :: String -> Int -> [Rewrite] -> [Int] -> [String]
showRewrite node i [] edges = [node ++ show i ++ "->n" ++ show e ++ "0" | e <- edges]
showRewrite node i (Rewrite a b:xs) edges = 
    [node ++ show i ++ "->" ++ node ++ show (i+1)
    ,node ++ show (i+1) ++ "->" ++ node ++ show (i+2) ++ "[color=\"blue\"]"
    ] ++
    showGExp (node ++ show (i+1)) a ++
    showGExp (node ++ show (i+2)) b ++
    showRewrite node (i+2) xs edges

showRewrite node i (GraphEnd:xs) edges = 
    [node ++ show i ++ "->" ++ node ++ show (i+1)
    ,node ++ show (i+1) ++ "[label=\"End\",color=\"red\"]"
    ] ++
    showRewrite node (i+1) xs edges



showGExp :: String -> GExp -> [String]
showGExp nn gexp = [nn ++ "[label=\"" ++ noQuotes (show gexp) ++ "\"]"]
    where
        noQuotes x = map f x
        f '\"' = '\''
        f x = x

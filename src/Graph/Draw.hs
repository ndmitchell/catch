
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
        nod :: Int -> String
        nod n = "n" ++ show n
    
        f n (Node name [] [GraphEnd]) = [nod n ++ "[label=\"End\",color=\"red\"]"]
        f n (Node name edges rewrite) = (nn ++ "[label=\"" ++ show n ++ "," ++ name ++ "\"]")
                                       : map (g nn) edges
                                       ++ showRewrite nn rewrite
            where nn = nod n
        
        g from to = from ++ "->" ++ nod to ++ "[label=\"\"]"

showRewrite :: String -> [Rewrite] -> [String]
showRewrite node [] = []
showRewrite node [Rewrite a b] = 
    [node ++ "->" ++ node ++ "e[label=\"E\"]"
    ,node ++ "->" ++ node ++ "b[label=\"B\"]"
    ]
    ++ showGExp (node ++ "b") a
    ++ showGExp (node ++"e") b



showGExp :: String -> GExp -> [String]
showGExp nn gexp = [nn ++ "[label=\"" ++ noQuotes (show gexp) ++ "\"]"]
    where
        noQuotes x = map f x
        f '\"' = '\''
        f x = x


module Graph.Draw(drawGraph) where

import Graph.Type
import Graph.Show


drawGraph :: Graph -> FilePath -> IO ()
drawGraph graph file = return ()



showGraph :: Graph -> String
showGraph (Graph xs) = unlines $ ["Digraph catch{"] ++ concat (zipWith f [0..] xs) ++ ["}"]
    where
        nod n = "n" ++ show n
    
        f n (Node edges Nothing) = nod n : map (g (nod n)) edges
        f n (Node edges (Just (Rewrite a b))) =
              [  nn ""
              , nn "" ++ "->" ++ nn "s" ++ "[label=\"S\"]"
              , nn "s" ++ "->" ++ nn "e" ++ "[label=\"E\"]"
              , nn "e" ++ "->" ++ nn "f"
              ]
              ++ map (g (nn "f")) edges
              ++ showGExp (nn "s") a
              ++ showGExp (nn "e") b
            where nn s = nod n ++ s
        
        g from to = from ++ "->" ++ nod (show to)


showGExp :: String -> GExp -> [String]
showGExp nn gexp = [nn ++ "->" ++ nn ++ "1"
                   ,nn ++ "1[label=\"" ++ show gexp ++ "\"]"
                   ]


module Graph.Solve(solveGraph) where

import Graph.Type
import Graph.Draw
import Graph.Rename

import Hite
import Data.Maybe
import Data.List
import Debug.Trace
import Pred.Predicate
import Control.Exception(assert)
import Directory
import Monad
import General.General


solveGraph :: String -> Hite -> Graph -> IO Bool
solveGraph file hite graph =
    do
        b <- doesDirectoryExist path
        when (not b) $ createDirectory path
        
        draw graph 0
        f graph 1
    where
        path = "Logs/" ++ file
    
        draw :: Graph -> Int -> IO ()
        draw graph n = drawGraph graph (path ++ "/" ++ make3 (show n))
        
        make3 x = replicate (3 - length x) '0' ++ x
    
        f :: Graph -> Int -> IO Bool
        f graph n | n > 15 = putStrLn "Time bound exceeded" >> return False
        f graph n = do let g = simplify graph
                       putStrLn $ show n ++ " simplifying..."
                       draw g n
                       if isSolved g then
                           return True
                        else if not $ canInstantiate g then
                           return False
                        else do
                           let g2 = instantiate hite g
                           putStrLn $ show (n+1) ++ " instantiating..."
                           draw g2 (n+1)
                           f g2 (n+2)


canInstantiate :: Graph -> Bool
canInstantiate graph = any f (concatMap rewrite graph)
    where
        f (Rewrite a b) = any isGFunc $ allGExp b
        f _ = False



isSolved :: Graph -> Bool
isSolved graph = not $ any isGraphEnd $ concatMap rewrite graph


---------------------------------------------------------------------
-- INSTANTIATE
-- instantiate a graph with functional forms

instantiate :: Hite -> Graph -> Graph
instantiate hite = evaluate hite . expand hite . onerewrite . funcsimp


-- each rewrite may have a maximum of one function call in the RHS
funcsimp :: Graph -> Graph
funcsimp graph = map f graph
    where
        f node = node{rewrite = concatMap g (rewrite node)}
        
        g rew@(Rewrite (GCtor "." l) (GCtor "." r))
                | null pfuncs = assert (null cfuncs) [rew]
                | length (pfuncs++cfuncs) == 1 = [rew]
                | otherwise = Rewrite (GCtor "." l) (GCtor "." (pfunc:l))
                            : g (Rewrite (GCtor "." (GVar newvar:l)) (mapGExp change (GCtor "." r)))
            where
                pfunc = head pfuncs
                (pfuncs,cfuncs) = partition isPureFunc $ filter isGFunc $ concatMap allGExp r
                isPureFunc (GFunc _ x) = isPure x
                newvar = head $ ["n" ++ show i | i <- [1..]] \\ nub [x | GVar x <- concatMap allGExp (l++r)]
                
                change x | x == pfunc = GVar newvar
                         | otherwise = x
        
        g x = [x]


-- each node must have a maximum of one rewrite expression
-- this probably does a lot of work that the control expansion spent
-- a while doing
-- also introduce a "zero" node
onerewrite :: Graph -> Graph
onerewrite graph = f (length graph) graph
    where
        f n [] = []
        f n (x:xs) | lrews <= 1 = x : f n xs
                   | otherwise = x{rewrite=[head rews], edges=[n]} :
                                 f (n+lrews) (
                                     xs ++
                                     [Node "" [m+1] [r] | (m,r) <- zip [n..] (tail rews)] ++
                                     [Node "" (edges x) []]
                                 )
            where
                lrews = length rews
                rews = rewrite x
                


-- for all functions, instantiate them to ensure that
-- any appropriate evaluations can be performed
expand :: Hite -> Graph -> Graph
expand hite graph = newzero : (map change $ concat newnodes)
    where
        newzero = Node "" (fromJust $ lookup 0 rep) []
        newnodes = map doNode graph
        rep = f 1 $ zip [0..] newnodes
            where
                f n [] = []
                f n ((m,x):xs) = (m, [n..lx-1]) : f lx xs
                    where lx = n + length x
    
        change node = node{edges = concatMap f (edges node)}
            where f n = fromJust $ lookup n rep
    
        doNode :: Node -> [Node]
        doNode node = map (\x -> node{rewrite=x}) $ doRewrites (rewrite node)
        
        doRewrites :: [Rewrite] -> [[Rewrite]]
        doRewrites x = crossProduct $ map doRewrite x
        
        doRewrite :: Rewrite -> [Rewrite]
        doRewrite r@(Rewrite lhs rhs) | any isGFunc $ allGExp rhs = instan lhs rhs
        doRewrite r = [r]
        
        -- instantiate any functions so they can be evaluated
        -- should be exactly one function here
        instan :: GExp -> GExp -> [Rewrite]
        instan lhs rhs | null renames = error "No valid renames!"
                       | otherwise = [Rewrite (applyRename r lhs) (applyRename r rhs) | r <- renames]
            where
                renames = filter validRename $ bindings root
                [root] = filter isGFunc $ allGExp rhs

        bindings :: GExp -> [Rename]
        bindings (GFunc name arg) = map (getRename hite func arg) opts
            where
                opts = [predAnd [a, varsToPred b] | let MCase alts = body func, MCaseAlt a b <- alts]
                func = getFunc hite name

        -- some guards are removed for being simple
        -- add all these back in
        varsToPred :: Expr -> Pred MCaseOpt
        varsToPred x = predAnd $ map predLit $ concat [ensurePath y | y <- allExpr x, isSel y]
        
        ensurePath :: Expr -> [MCaseOpt]
        ensurePath (Var _) = []
        ensurePath (Sel x y) = MCaseOpt x (ctorName $ getCArg hite y) : ensurePath x
        

-- evaluate all functions, every function must be evaluated
evaluate :: Hite -> Graph -> Graph
evaluate hite graph = map f graph
    where
        f node = node{rewrite = map g (rewrite node)}
        
        g (Rewrite l r) = Rewrite l (mapGExp (eval hite) r)
        g x = x


eval :: Hite -> GExp -> GExp
eval hite x | not (isGFunc x) = x
eval hite orig@(GFunc name (GCtor "." cargs)) =
        if null res then error $ show ("Graph.Solve.eval: no matches",{-name,-}cargs)
        else assert (length res == 1) (head res)
    where
        (Func _ args (MCase opts) _) = getFunc hite name
        res = [convert x | MCaseAlt p x <- opts,
                    isTrue (mapPredLit f p), -- demand the guard is met
                    all (isJust . getVar) [y | y <- allExpr x, isSel y || isVar y]] -- and that the var's exist
        
        f :: MCaseOpt -> Pred ()
        f (MCaseOpt x c) = predBool $ case getVar x of
                               Just (GCtor n _) | n == c -> True
                               _ -> False
      
        getVar :: Expr -> Maybe GExp
        getVar x = followPath (pickVar var) path
            where (var,path) = getVarPath x

        pickVar :: FuncArg -> GExp
        pickVar x = cargs !! a
            where (Just a) = elemIndex x args
            
        followPath x [] = Just x
        followPath (GCtor n x) (y:ys) | isJust q = followPath (x !! fromJust q) ys
            where
                ctor = getCtor hite n
                q = elemIndex y (ctorArgs ctor)
        followPath _ _ = Nothing
                

        convert :: Expr -> GExp
        convert (Make a b) = GCtor a (map convert b)
        convert (Call (CallFunc a) b) = GFunc a (GCtor "." (map convert b))
        convert (Msg x) = GVar "string"
        convert x = case getVar x of
                        Just y -> y
                        Nothing -> error $ "Graph.Solve.evaluate.convert, Nothing from " ++ show (name,{-x,-}cargs)


---------------------------------------------------------------------
-- SIMPLIFY
-- simplify entirely on a graph, must be reducing and terminating
simplify :: Graph -> Graph
simplify = edgeIncompatible . graphItemDelete . graphControlDelete . graphItemDelete . graphControlDelete . graphItemDelete . graphItemDelete . graphControlDelete . graphItemDelete



-- * Item removal
-- Delete all redundant bits

graphItemDelete :: Graph -> Graph
graphItemDelete = edgeIncompatible . compressList . removeSimpleRewrite . reachFailure

-- is an edge impossible to reach, because the nodes either side are incompatible
edgeIncompatible :: Graph -> Graph
edgeIncompatible graph = map f graph
    where
        f node = node{edges = filter (isComp node) (edges node)}
        
        isComp n1 nn2 = isCompatibleNode n1 n2
            where n2 = graph !! nn2


-- try and do some basic simplifications
compressList :: Graph -> Graph
compressList graph = map f graph
    where
        f n = if any isGraphBreak rew then n{edges=[], rewrite=[GraphBreak]} else n{rewrite=rew}
            where rew = pairs $ map simp $ rewrite n
        
        simp (Rewrite a b) = Rewrite (onlyVar vars a) (onlyVar vars b)
            where vars = allVars a `intersect` allVars b
        simp x = x

        
        pairs [] = []
        pairs [x] = [x]
        pairs (Rewrite a b:GraphEnd:xs) = Rewrite (onlyVar [] a) GFree:GraphEnd:[]
        pairs (Rewrite a1 b1:Rewrite a2 b2:xs) | not (isCompatibleExp b1 a2) = Rewrite a1 b1 : GraphBreak : []
                                               | isPure b1 = pairs (fuse (Rewrite a1 b1) (Rewrite a2 b2) : xs)
        pairs (a:b:xs) = a : pairs (b:xs)

        
        allVars :: GExp -> [String]
        allVars x = nub [n | GVar n <- allGExp x]
        
        -- not sure this transformation is valid
        onlyVar :: [String] -> GExp -> GExp
        onlyVar vars x = x -- mapGExp f x
            where
                f (GVar n) | not (n `elem` vars) = GFree
                f x = x


fuse :: Rewrite -> Rewrite -> Rewrite
fuse (Rewrite l1 r1) (Rewrite l2 r2) = Rewrite (rep unifyL l1) (rep unifyR r2)
    where
        unifyL = filter (not . simpleUnify) $ unify r1 l2
        unifyR = unify l2 r1
    
        -- one sided unify
        unify :: GExp -> GExp -> [(String, GExp)]
        unify (GCtor n1 x1) (GCtor n2 x2) = assert (n1 == n2 && length x1 == length x2) $
            concatMap (uncurry unify) (zip x1 x2)
        unify (GVar x) y = [(x,y)]
        unify _ _ = []
        
        simpleUnify (x, GVar y) = True
        simpleUnify _ = False
        
        rep :: [(String, GExp)] -> GExp -> GExp
        rep dict x = mapGExp f x
            where
                f (GVar n) = case lookup n dict of
                                Just y -> y
                                Nothing -> GVar n
                f x = x


-- can the two GExp's be unified, with matching contstructors
isCompatibleExp :: GExp -> GExp -> Bool
isCompatibleExp (GCtor n1 x1) (GCtor n2 x2) = n1 == n2 && length x1 == length x2 && and (zipWith isCompatibleExp x1 x2)
isCompatibleExp _ _ = True

isCompatibleRewrite :: Rewrite -> Rewrite -> Bool
isCompatibleRewrite (Rewrite _ r) (Rewrite l _) = isCompatibleExp r l
isCompatibleRewrite _ _ = True

isCompatibleNode :: Node -> Node -> Bool
isCompatibleNode n1 n2 | not (null r1) && not (null r2) = isCompatibleRewrite (last r1) (head r2)
    where (r1, r2) = (rewrite n1, rewrite n2)
isCompatibleNode _ _ = True



-- is an expression pure, i.e. no function calls
isPure :: GExp -> Bool
isPure x = null [() | GFunc _ _ <- allGExp x]



-- if a rewrite changes nothing, and has not ctor's apart from the base "."
-- then it is redundant
removeSimpleRewrite :: Graph -> Graph
removeSimpleRewrite nodes = map f nodes
    where
        f n = n{rewrite = filter (not . isSimple) (rewrite n)}
        
        isSimple (Rewrite (GCtor "." a) (GCtor "." b))
            = all isGVar (a ++ b) && map fromGVar a == map fromGVar b
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
graphControlDelete = {- yPromotion . -} labelInline {- . controlReduce -}


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


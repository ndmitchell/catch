-- Reduction.hs - 1997, 2001 Thorsten Brehm
-- parsing a string to a syntax tree and reducing it using
-- a parallel or leftmost strategy.

import List

-- ## Definitions ##########################################################

-- Contructors for syntax tree of a term
data Term a = Var a | Const String | Appl  String [Term a] deriving Show

-- Types
type Vars      = [String]
type SigSymbol = (String,Int)
type Signature  = [SigSymbol]

-- Strategy: which position(s) is to be reduced next?
type Strategy a = (Term a -> [Position])
type Position = [Int]

type VarMapping a b = [(a,b)]

type Operation a = ([a]->a)
type Algebra a  = [(SigSymbol,Operation a)]


-- ## Pretty Printer #######################################################

-- concatenate a list of strings, separated by a special separator-string
inconcat :: String -> [String] -> String
inconcat s []     = ""
inconcat s (a:[]) = a
inconcat s (a:l)  = a ++ s ++ (inconcat s l)

-- transform syntax tree into a string
unparseterm :: (a -> String) -> Term a -> String
unparseterm s (Var v)     = s v
unparseterm s (Const c)   = c
unparseterm s (Appl f as) = f ++ "(" ++ (inconcat "," (map (unparseterm s) as)) ++ ")" 

-- pretty print a list of terms
prettyprint list = foldr (\x y-> (print (unparseterm (\x -> (show x)) x)) >> y)
                         (print "finished")
                         list

-- ## Parsing a term #######################################################

-- split a string of argument definitions into a list of strings
splitargs :: String -> Int -> String -> [String]
splitargs [] 0 x       = [x]
splitargs (',':l) 1 x  = (x:splitargs l 1 "")
splitargs (')':l) 1 x  = splitargs l 0 x
splitargs (')':l) n x  = splitargs l (n-1) (x++")")
splitargs ('(':l) n x  = splitargs l (n+1) (x++"(")
splitargs (a:l) n x    = splitargs l n (x++[a])
splitargs  _ _ _       = error "Unable to parse arguments. Maybe ( ) mismatch?"

-- split string into a tuple, containing a string for a function/constant name and
-- a list of strings of its arguments
splitterm :: String -> (String,[String])
splitterm []      = ([],[])
splitterm ('(':l) = ([],splitargs l 1 "")
splitterm (x:l)   = ((x:h),t) where (h,t) = splitterm l
splitterm _      = error "Unable to parse term. Unknown function/constant/variable name?"


-- parse a whole string into a syntax tree, respective to variables and a signature
parseterm :: Vars -> Signature -> String -> Term String
parseterm vars ome str | (t==[] && (elem h vars)) = Var h
                       | (t==[] && (elem (h,0) ome)) = Const h
                       | (elem (h,(length t)) ome) = Appl h (map (parseterm vars ome) t)
                       | otherwise = error "Unable to parse term. String is not a valid term! Maybe bad arity?"
                where (h,t) = splitterm str


-- ## helpful functions for reduction

-- lookup value of a variable in the variablemapping
lookup' :: Eq a => a -> [(a,b)] -> b
lookup' v []        = error "No value for variable found!"
lookup' v ((w,x):l) | w==v = x
                    | otherwise = lookup' v l 

-- map a term over variables to a term over values. substitute variables by their values.
mapped :: Eq a => VarMapping a b -> Term a -> Term b
mapped beta (Var v)       = let  l = lookup' v beta in
                   Var l
mapped beta (Const c)     = Const c
mapped beta (Appl f args) = Appl f (map (mapped beta) args)


-- get term at a certain position
partialterm :: Position -> Term a -> Term a
partialterm [] v = v
partialterm (n:l) (Appl f args) = partialterm l (args!!(n-1))
partialterm _ _  = error "bad position - there's no such position, hence no term at this position..."

-- substitute a term at a position with a new term
partialsubst :: Position -> Term a -> Term a -> Term a
partialsubst [] v w = w
partialsubst (n:l) (Appl f args) w       
       | n<=(length args) =
           Appl f ((take (n-1) args)++[(partialsubst l (args!!(n-1)) w)]++
               (drop n args))
partialsubst _ _ _ = error "bad position - substitution failed"



-- ## reduction strategies ###############################################

-- parallel reduction strategy, reduce all outermost redexes

parallel' :: Position -> Strategy a
parallel' p (Var v)       = []
parallel' p (Const c)     = [p]   
parallel' p (Appl f args) | all irreducible args = [p]
                          | otherwise 
                             = concat
                                (map (uncurry parallel') 
                                     (zip [p++[(length args)-i+1] | i<-[1..(length args)]] 
                                          (reverse args)
                                     ))
    
parallel :: Strategy a
parallel = parallel' []

-- leftmost reduction strategy
leftmost :: Strategy a
leftmost t | par==[] = []
           |otherwise = [head (sort par)]  -- only reduce the leftmost position!
           where par = parallel t



-- ## reduction semantics ###############################################

-- is term irreducible?
irreducible :: Term a -> Bool
irreducible (Var v) = True
irreducible _       = False     

-- reduction rules
redrule :: Algebra a -> Term a -> Term a
redrule alg (Const c)     = let  l = lookup' (c,0) alg in 
                Var (l [])
redrule alg (Appl f args) = let  l = lookup' (f,length args) alg
    in
      Var (l (map (\(Var v)->v) args))
redrule _ _ = error "No reduction rule matches."


-- reduction relation
redrel :: Algebra a -> [Position] -> Term a -> Term a
redrel a []    t = t
redrel a (p:l) t = partialsubst p t' (redrule a (partialterm p t'))
                    where t' = (redrel a l t)

redsemantics_ :: Algebra a -> Strategy a -> Term a -> [Term a]
redsemantics_ a s t | irreducible t = [t]
                    | otherwise = let t' = redrel a (s t) t   
                       in (t:redsemantics_ a s t')

redsemantics :: Eq a => Algebra b -> VarMapping a b -> Strategy b -> Term a -> [Term b]
redsemantics a beta s t = redsemantics_ a s (mapped beta t)


-- ## examples using the module ########################################

-- define a signature with operational symbols
signature :: Signature
signature = [("add",2),("mult",2),("suc",1),("zero",0)]

-- define an algebra for this signature
algebra :: Algebra Int
algebra = [(("add",2),\[x,y]->x+y), (("mult",2),\[x,y]->x*y),
           (("suc",1),\[x]->x+1),
           (("zero",0),\[]->0)]

-- allowed variables
vars = ["x", "y"]
varmapping = [("x",3), ("y",2)]

--leftmost and parallel reduction semantics
leftmost_sem term = redsemantics algebra varmapping leftmost term
parallel_sem term = redsemantics algebra varmapping parallel term

-- parse a string to a term with variables (vars), and signature
parse string = parseterm vars signature string

-- parse an example string to a term (syntax tree)
t = parse "add(add(suc(zero),zero),suc(suc(mult(x,y))))"

-- ## main ##############################################################
-- reduce term using leftmost and parallel reduction
main =
        (print "leftmost reduction: ") >> prettyprint (leftmost_sem t)
        >>
        (print "parallel reduction: ") >> prettyprint (parallel_sem t)


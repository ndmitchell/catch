import List

-- A Brute-Force SAT Solver (I deliberately avoid partial assignments!)

type Var           =  Int

data Lit           =  Pos Var | Neg Var

type CNF           =  [[Lit]]

type Choice        =  [(Var,Bool)]

var                :: Lit -> Var
var (Pos v)        =  v
var (Neg v)        =  v

vars               :: CNF -> [Var]
vars               =  nub . concatMap (map var)

choices            :: [Var] -> [Choice]
choices []         =  [[]]
choices (v:vs)     =  [(v,b):c | b <- bool, c <- choices vs]
  where bool       =  [False, True]

val                :: Choice -> Var -> Bool
val c v            =  head [b | (w,b) <- c, v == w]

eval               :: Choice -> Lit -> Bool
eval c (Pos v)     =  val c v
eval c (Neg v)     =  not (val c v)

valid              :: CNF -> Choice -> Bool
valid f c          =  all (or . map (eval c)) f

sat                :: CNF -> Bool
sat f              =  any (valid f) (choices (vars f))

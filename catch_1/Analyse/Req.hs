
module Analyse.Req where


data Req a = a :< Constraint


type Constraint = [Val]

data Val    =  [Match] :* [Match]
            |  Any
data Match  =  Match CtorName [Val]

-- useful auxiliaries, non recursive fields
nonRecs :: CtorName -> [Int]
nonRecs c = [i | i <- [0..arity c-1], not (isRec (c,i))]

-- a complete Match on |c|
complete :: CtorName -> Match
complete c = Match c (map (const Any) (nonRecs c))

notin :: CtorName -> Constraint
notin c = map complete (delete c cs) :* map complete cs
    where cs = ctors c

(|>) :: Field -> Constraint -> Constraint
(c,i) |> k = notin c ++ map f k
    where
    f Any = Any
    f (ms1 :* ms1) | isRec (c,i) = complete c :* merge ms1 ms2
    f v =  Match c [if i == j then v else Any | j <- nonRecs c]
           :* map complete (ctors c)

(<|) :: CtorName -> Constraint -> Prop (Req Int)
c <| vs = or (map f vs)
    where
    (rec,non) = partition (isRec . (,) c) [0..arity c-1]

    f Any = Any
    f (ms1 :* ms2) = or [g vs | Match c1 vs1 <- ms1, c1 == c]
        where g vs =  and (zipWith (:<) non vs) &&
                      and (map (:< (ms2 :* ms2)) rec)

mergeVal :: Val -> Val -> Val
(a1 :* b1) `mergeVal` (a2 :* b2) = merge a1 a2 :* merge b1 b2

merge :: [Match] -> [Match] -> [Match]
merge  ms1 ms2 = [Match c1 (zipWith (`mergeVal`) vs1 vs2) |
       Match c1 vs1 <- ms1, Match c2 vs2 <- ms2, c1 == c2]


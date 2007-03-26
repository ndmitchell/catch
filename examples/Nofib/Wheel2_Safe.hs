-- Mark II lazy wheel-sieve.
-- Colin Runciman (colin@cs.york.ac.uk); March 1996.
-- See article "Lazy wheel sieves and spirals of primes" (to appear, JFP).

module Wheel2_Safe where

primes :: [Int]
primes = spiral wheels primes squares

spiral (Wheel s ms ns:ws) ps qs =
  foldr turn0 (roll s) ns
  where
  roll o = foldr (turn o) (foldr (turn o) (roll (o+s)) ns) ms
  turn0  n rs =
    if n<q then n:rs else sp
  turn o n rs =
    let n' = o+n in
    if n'==2 || n'<q then n':rs else dropWhile (<n') sp
  sp = spiral ws (drop 1 ps) (drop 1 qs)
  q = if null qs then 0 else head qs
spiral _ _ _ = []

squares :: [Int]
squares = [p*p | p <- primes]

data Wheel = Wheel Int [Int] [Int]

wheels :: [Wheel]
wheels = Wheel 1 [1] [] :
         zipWith3 nextSize wheels primes squares 

nextSize (Wheel s ms ns) p q =
  Wheel (s*p) ms' ns'
  where
  (xs, ns') = span (<=q) (foldr turn0 (roll (p-1) s) ns)
  ms' = foldr turn0 xs ms
  roll 0 _ = []
  roll t o = foldr (turn o) (foldr (turn o) (roll (t-1) (o+s)) ns) ms
  turn0  n rs =
    if n`modSafe`p>0 then n:rs else rs
  turn o n rs =
    let n' = o+n in
    if n'`modSafe`p>0 then n':rs else rs

modSafe x y = if y == 0 then 0 else x

main n = print (primes)

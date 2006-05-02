-- Here's a small example program (previously used to illustrate fault-finding using Hat)
-- that seems to cause a blow-up of constraints in pre-alpha catch.

-- Tautology testing by partial evaluation and case analysis.
-- Colin Runciman, 2003.

module Taut where

data Prop = Lit Bool
          | Var Char
          | Not Prop
  | Imp Prop Prop

eval :: Prop -> Prop
eval (Lit b)   = Lit b
eval (Var v)   = Var v
eval (Not p)   = case eval p of
                    Lit b -> Lit (not b)
                    p'    -> Not p'
eval (Imp p q) = case (eval p, eval q) of
                    (Lit b, q') -> if b then q' else Lit True
                    (p', Lit b) -> if b then Lit True else Not p'
                    (p',    q') -> Imp p q
                                     --             XXXXXXX
                                     -- deliberate mistake: should be Imp p' q'

varOf :: Prop -> Char
varOf (Var v)   = v
varOf (Not p)   = varOf p
varOf (Imp p _) = varOf p

subst :: Char -> Bool -> Prop -> Prop
subst _ _ (Lit b)   = Lit b
subst v b (Var w)   = if v==w then Lit b else Var w
subst v b (Not p)   = Not (subst v b p)
subst v b (Imp p q) = Imp (subst v b p) (subst v b q)

taut :: Prop -> Bool
taut p = case eval p of
             Lit b -> b
             p'    -> let v = varOf p' in
                  taut (subst v True  p') &&
                  taut (subst v False p')

main :: Bool
main = taut (Imp (Var 'p') (Imp (Imp (Var 'p') (Var 'q')) (Var 'q')))


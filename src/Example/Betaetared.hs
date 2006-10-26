{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{-# OPTIONS_YHC -cpp -underscore #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}
{- This program is free software; you can redistribute it and/or      -}
{- modify it under the terms of the GNU Lesser General Public License -}
{- as published by the Free Software Foundation; either version 2.1   -}
{- of the License, or (at your option) any later version.             -}
{-                                                                    -}
{- This program is distributed in the hope that it will be useful,    -}
{- but WITHOUT ANY WARRANTY; without even the implied warranty of     -}
{- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      -}
{- GNU General Public License for more details.                       -}
{-                                                                    -}
{- You should have received a copy of the GNU Lesser General Public   -}
{- License along with this program; if not, write to the Free         -}
{- Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA -}
{- 02110-1301 USA                                                     -}

module Betaetared where

import qualified Prelude
import Prelude(Int,read,print,pred,succ,($!))
import System.Environment



#if defined(__GLASGOW_HASKELL__)
import qualified GHC.Base
unsafeCoerce = GHC.Base.unsafeCoerce#
#elif defined(__YHC__)
import qualified YHC.Primitive
unsafeCoerce = YHC.Primitive.unsafeCoerce
#else
-- HUGS
import qualified Hugs.IOExts
unsafeCoerce = Hugs.IOExts.unsafeCoerce
#endif

i2n :: Int -> Nat
i2n 0 = O 
i2n n = S (i2n (pred n))

decode_church l = decode_churchH l 0
decode_churchH (Abs _ t) x = decode_churchH t x
decode_churchH (App _ t) x = decode_churchH t $! succ x
decode_churchH (Var _) x = x

main = do
  [a1,a2] <- getArgs
  let n = i2n (read a1) 
  let m = i2n (read a2)
  print (decode_church (church_power n m))


__ = Prelude.error "Logical or arity value used"

false_rect :: (()->()) -> a1
false_rect _ =
  Prelude.error "absurd case"

eq_rect :: a1 -> a2 -> a1 -> a2
eq_rect x f y =
  f

eq_rec :: a1 -> a2 -> a1 -> a2
eq_rec x f y =
  eq_rect x f y

eq_rec_r :: a1 -> a2 -> a1 -> a2
eq_rec_r x h y =
  eq_rec x h y

eq_rect_r :: a1 -> a2 -> a1 -> a2
eq_rect_r x h y =
  eq_rect x h y

data Bool = True
            | False

data Nat = O
           | S Nat

nat_rect :: a1 -> (Nat -> a1 -> a1) -> Nat -> a1
nat_rect f f0 n =
  case n of
    O -> f
    S n0 -> f0 n0 (nat_rect f f0 n0)

nat_rec :: a1 -> (Nat -> a1 -> a1) -> Nat -> a1
nat_rec f f0 n =
  nat_rect f f0 n

type Sig a = a
  -- singleton inductive, whose constructor was exist
  
data Sumbool = Left
               | Right

sumbool_rect :: ((()->()) -> a1) -> ((()->()) -> a1) -> Sumbool -> a1
sumbool_rect f f0 s =
  case s of
    Left -> f __
    Right -> f0 __

sumbool_rec :: ((()->()) -> a1) -> ((()->()) -> a1) -> Sumbool -> a1
sumbool_rec f f0 s =
  sumbool_rect f f0 s

plus :: Nat -> Nat -> Nat
plus n m =
  case n of
    O -> m
    S p -> S (plus p m)

minus :: Nat -> Nat -> Nat
minus n m =
  case n of
    O -> O
    S k -> (case m of
              O -> S k
              S l -> minus k l)

eq_nat_dec :: Nat -> Nat -> Sumbool
eq_nat_dec n m =
  nat_rec (\m0 -> nat_rec Left (\m1 iHm -> Right) m0) (\n0 iHn m0 ->
    nat_rec Right (\m1 iHm ->
      sumbool_rec (\_ -> Left) (\_ -> Right) (iHn m1)) m0) n m

le_lt_dec :: Nat -> Nat -> Sumbool
le_lt_dec n m =
  nat_rec (\m0 -> Left) (\n0 iHn m0 ->
    nat_rec Right (\m1 iHm ->
      sumbool_rec (\_ -> Left) (\_ -> Right) (iHn m1)) m0) n m

ifb :: Bool -> Bool -> Bool -> Bool
ifb b1 b2 b3 =
  case b1 of
    True -> b2
    False -> b3

andb :: Bool -> Bool -> Bool
andb b1 b2 =
  ifb b1 b2 False

orb :: Bool -> Bool -> Bool
orb b1 b2 =
  ifb b1 True b2

negb :: Bool -> Bool
negb b =
  case b of
    True -> False
    False -> True

data List a = Nil
              | Cons a (List a)

list_rect :: a2 -> (a1 -> (List a1) -> a2 -> a2) -> (List a1) -> a2
list_rect f f0 l =
  case l of
    Nil -> f
    Cons a l0 -> f0 a l0 (list_rect f f0 l0)

length :: (List a1) -> Nat
length l =
  case l of
    Nil -> O
    Cons a m -> S (length m)

app :: (List a1) -> (List a1) -> List a1
app l m =
  case l of
    Nil -> m
    Cons a l1 -> Cons a (app l1 m)

nth :: Nat -> (List a1) -> a1 -> a1
nth n l default0 =
  case n of
    O -> (case l of
            Nil -> default0
            Cons x l' -> x)
    S m -> (case l of
              Nil -> default0
              Cons x t -> nth m t default0)

map :: (a1 -> a2) -> (List a1) -> List a2
map f l =
  case l of
    Nil -> Nil
    Cons a t -> Cons (f a) (map f t)

iter_nat :: Nat -> (a1 -> a1) -> a1 -> a1
iter_nat n f x =
  case n of
    O -> x
    S n' -> f (iter_nat n' f x)

seq :: Nat -> Nat -> List Nat
seq start len =
  case len of
    O -> Nil
    S len0 -> Cons start (seq (S start) len0)

consn :: Nat -> a1 -> (List a1) -> List a1
consn n a l =
  iter_nat n (\x -> Cons a x) l

data Type = Iota
            | Arrow Type Type

type_rect :: a1 -> (Type -> a1 -> Type -> a1 -> a1) -> Type -> a1
type_rect f f0 t =
  case t of
    Iota -> f
    Arrow t0 t1 -> f0 t0 (type_rect f f0 t0) t1 (type_rect f f0 t1)

type_rec :: a1 -> (Type -> a1 -> Type -> a1 -> a1) -> Type -> a1
type_rec f f0 t =
  type_rect f f0 t

data Term = Var Nat
            | App Term Term
            | Abs Type Term

term_rect :: (Nat -> a1) -> (Term -> a1 -> Term -> a1 -> a1) -> (Type -> Term
             -> a1 -> a1) -> Term -> a1
term_rect f f0 f1 t =
  case t of
    Var n -> f n
    App t0 t1 -> f0 t0 (term_rect f f0 f1 t0) t1 (term_rect f f0 f1 t1)
    Abs t0 t1 -> f1 t0 t1 (term_rect f f0 f1 t1)

term_rec :: (Nat -> a1) -> (Term -> a1 -> Term -> a1 -> a1) -> (Type -> Term
            -> a1 -> a1) -> Term -> a1
term_rec f f0 f1 t =
  term_rect f f0 f1 t

dterm :: Term
dterm =
  Var O

dtype :: Type
dtype =
  Iota

arrow_right :: Type -> Type
arrow_right sigma =
  case sigma of
    Iota -> dtype
    Arrow t sigma0 -> sigma0

type_dec :: Type -> Type -> Sumbool
type_dec r s =
  type_rec (\s0 -> case s0 of
                     Iota -> Left
                     Arrow t t0 -> Right) (\t h t0 h0 s0 ->
    case s0 of
      Iota -> Right
      Arrow t1 t2 ->
        sumbool_rec (\_ ->
          eq_rec_r t1
            (sumbool_rec (\_ -> eq_rec_r t2 Left t0) (\_ -> Right) (h0 t2)) t)
          (\_ -> Right) (h t1)) r s

term_dec :: Term -> Term -> Sumbool
term_dec r s =
  term_rec (\n s0 ->
    case s0 of
      Var n0 ->
        sumbool_rec (\_ -> eq_rec_r n0 Left n) (\_ -> Right)
          (eq_nat_dec n n0)
      App t t0 -> Right
      Abs t t0 -> Right) (\t h t0 h0 s0 ->
    case s0 of
      Var n -> Right
      App t1 t2 ->
        sumbool_rec (\_ ->
          eq_rec_r t1
            (sumbool_rec (\_ -> eq_rec_r t2 Left t0) (\_ -> Right) (h0 t2)) t)
          (\_ -> Right) (h t1)
      Abs t1 t2 -> Right) (\t t0 h s0 ->
    case s0 of
      Var n -> Right
      App t1 t2 -> Right
      Abs t1 t2 ->
        sumbool_rec (\_ ->
          eq_rec_r t1
            (sumbool_rec (\_ -> eq_rec_r t2 Left t0) (\_ -> Right) (h t2)) t)
          (\_ -> Right) (type_dec t t1)) r s

occurs :: Nat -> Term -> Bool
occurs k r =
  case r of
    Var n -> (case eq_nat_dec n k of
                Left -> True
                Right -> False)
    App r0 s -> orb (occurs k r0) (occurs k s)
    Abs rho0 r0 -> occurs (S k) r0

up :: Nat -> Term -> Term
up l r =
  case r of
    Var n -> (case le_lt_dec l n of
                Left -> Var (S n)
                Right -> Var n)
    App r0 s -> App (up l r0) (up l s)
    Abs rho0 r0 -> Abs rho0 (up (S l) r0)

data Substitution = Mk_sub (List Term) Nat

support :: Substitution -> List Term
support s =
  case s of
    Mk_sub support0 shift0 -> support0

shift :: Substitution -> Nat
shift s =
  case s of
    Mk_sub support0 shift0 -> shift0

sublift :: Substitution -> Substitution
sublift rs =
  Mk_sub (Cons (Var O) (map (up O) (support rs))) (S (shift rs))

sub :: Term -> Substitution -> Term
sub r rs =
  case r of
    Var k ->
      nth k (support rs) (Var
        (plus (minus k (length (support rs))) (shift rs)))
    App r0 s -> App (sub r0 rs) (sub s rs)
    Abs rho0 r0 -> Abs rho0 (sub r0 (sublift rs))

id :: Nat -> Substitution
id k =
  Mk_sub (map (\x -> Var x) (seq O k)) k

swap0 :: Nat -> Substitution
swap0 k =
  Mk_sub (app (map (up O) (support (id k))) (Cons (Var O) Nil)) (S (S k))

sub_swap0 :: Term -> Nat -> Term
sub_swap0 r k =
  sub r (swap0 k)

type Context = List Type

typ :: Context -> Term -> Type
typ rhos r =
  case r of
    Var n -> nth n rhos dtype
    App r0 s -> arrow_right (typ rhos r0)
    Abs rho0 r0 -> Arrow rho0 (typ (Cons rho0 rhos) r0)

ext_ctx :: Context -> Nat -> Type -> List Type
ext_ctx rhos k rho0 =
  consn (minus (S k) (length rhos)) rho0 Nil

data Prod a b = Pair a b

prod_rect :: (a1 -> a2 -> a3) -> (Prod a1 a2) -> a3
prod_rect f p =
  case p of
    Pair x x0 -> f x x0

fst :: (Prod a1 a2) -> a1
fst ab =
  case ab of
    Pair a x -> a

snd :: (Prod a1 a2) -> a2
snd ab =
  case ab of
    Pair x b -> b

type Halfprod b = b
  -- singleton inductive, whose constructor was halfpair
  
halfprod_rect :: ((()->()) -> a1 -> a2) -> (Halfprod a1) -> a2
halfprod_rect f h =
  f __ h

type Requirements =
  Nat -> Type -> Term -> Term
  -- singleton inductive, whose constructor was Build_Requirements
  
abstr :: Requirements -> Nat -> Type -> Term -> Term
abstr r x x0 x1 =
  r x x0 x1

type SN = Nat -> (List Type) -> (()->()) -> Term

type SA = Nat -> (List Type) -> (()->()) -> Term

type SC = (()->())

sN_ext_ctx :: Requirements -> Type -> Context -> (List 
              Type) -> Term -> SN -> Nat -> (List Type) -> 
              Term
sN_ext_ctx r rho0 rhos sigmas r0 x k sigmas0 =
  eq_rec_r (app rhos (app sigmas sigmas0)) (x k (app sigmas sigmas0) __)
    (app (app rhos sigmas) sigmas0)

sC_ext_ctx :: Requirements -> Type -> Context -> (List 
              Type) -> Term -> SC -> SC
sC_ext_ctx r rho0 rhos sigmas r0 x =
  case rho0 of
    Iota ->
      halfprod_rect (\_ b ->
        unsafeCoerce (\x0 x1 _ -> sN_ext_ctx r Iota rhos sigmas r0 b x0 x1))
        (unsafeCoerce x)
    Arrow rho1 rho2 ->
      halfprod_rect (\_ b ->
        unsafeCoerce (\s sigmas0 x0 ->
          eq_rect_r (app rhos (app sigmas sigmas0))
            (b s (app sigmas sigmas0)
              (eq_rect (app (app rhos sigmas) sigmas0) x0
                (app rhos (app sigmas sigmas0))))
            (app (app rhos sigmas) sigmas0))) (unsafeCoerce x)

one :: Requirements -> Type -> Context -> Term -> Prod (SC -> SN) (SA -> SC)
one r rho0 rhos r0 =
  type_rect (\rhos0 r1 _ -> Pair (\x ->
    halfprod_rect (\_ b -> b) (unsafeCoerce x)) (\h1 ->
    unsafeCoerce (\k sigmas _ -> h1 k sigmas __)))
    (\rho1 iHrho1 rho2 iHrho2 rhos0 r1 _ -> Pair (\x k mus _ ->
    let rhos' = app rhos0 mus in
    let sigmas = ext_ctx rhos' k rho1 in
    abstr r k rho1
      (fst (iHrho2 (app rhos' sigmas) (App r1 (Var k)) __)
        (eq_rect_r (app rhos0 (app mus sigmas))
          (unsafeCoerce x (Var k) (app mus sigmas)
            (snd (iHrho1 (app rhos0 (app mus sigmas)) (Var k) __)
              (eq_rect (app (app rhos0 mus) sigmas) (\k0 sigmas0 _ -> Var k)
                (app rhos0 (app mus sigmas))))) (app (app rhos0 mus) sigmas))
        (S k) Nil __)) (\h1 ->
    unsafeCoerce (\s sigmas x ->
      snd (iHrho2 (app rhos0 sigmas) (App r1 s) __) (\k sigmas0 _ -> App
        (h1 k (app sigmas sigmas0) __)
        (fst (iHrho1 (app rhos0 sigmas) s __) x k sigmas0 __))))) rho0 rhos
    r0 __

two :: Requirements -> Type -> Context -> Term -> Term -> SC -> SC
two r rho0 rhos r0 r' x =
  type_rect (\rhos0 r1 r'0 _ h1 _ ->
    unsafeCoerce (\k sigmas _ -> unsafeCoerce h1 k sigmas __))
    (\rho1 iHrho1 rho2 iHrho2 rhos0 r1 r'0 _ x0 _ ->
    unsafeCoerce (\s sigmas x1 ->
      iHrho2 (app rhos0 sigmas) (App r1 s) (App r'0 s) __
        (halfprod_rect (\_ b -> b s sigmas x1) (unsafeCoerce x0)) __)) rho0
    rhos r0 r' __ x __

type SCs = (()->())

sCs_nth :: Requirements -> Context -> (List Type) -> Type -> (List 
           Term) -> Term -> Nat -> SCs -> SC
sCs_nth r sigmas rhos rho0 rs r0 n x =
  list_rect (\rho1 rs0 r1 n0 _ -> unsafeCoerce (\_ -> false_rect __))
    (\a rhos0 iHrhos rho1 rs0 r1 n0 _ ->
    case rs0 of
      Nil -> unsafeCoerce (\_ -> false_rect __)
      Cons t rs1 -> (\x0 ->
        case unsafeCoerce x0 of
          Pair s s0 ->
            (case n0 of
               O -> s
               S n1 -> iHrhos rho1 rs1 r1 n1 __ s0))) rhos rho0 rs r0 n __ x

sCs_ext_ctx :: Requirements -> Context -> (List Type) -> Context -> (List
               Term) -> SCs -> SCs
sCs_ext_ctx r sigmas sigmas0 rhos rs x =
  list_rect (\rs0 ->
    case rs0 of
      Nil -> unsafeCoerce __
      Cons x0 x1 -> unsafeCoerce __ x0 x1) (\a rhos0 iHrhos rs0 ->
    case rs0 of
      Nil -> unsafeCoerce __
      Cons t rs1 -> (\x0 ->
        prod_rect (\a0 b ->
          unsafeCoerce (Pair (sC_ext_ctx r a sigmas sigmas0 t a0)
            (iHrhos rs1 b))) (unsafeCoerce x0))) rhos rs x

three :: Requirements -> Term -> Substitution -> Context -> Context -> Type
         -> SCs -> SC
three r r0 rs rhos sigmas rho0 x =
  term_rect (\n rs0 rhos0 sigmas0 rho1 x0 _ ->
    eq_rect (nth n rhos0 dtype)
      (sCs_nth r sigmas0 rhos0 dtype (support rs0) (Var
        (plus (minus n (length (support rs0))) (shift rs0))) n x0) rho1)
    (\r1 iHr1 r2 iHr2 rs0 rhos0 sigmas0 rho1 x0 _ ->
    eq_rect (app sigmas0 Nil)
      (unsafeCoerce
        (iHr1 rs0 rhos0 sigmas0 (Arrow (typ rhos0 r2) rho1) x0 __)
        (sub r2 rs0) Nil
        (eq_rect sigmas0 (iHr2 rs0 rhos0 sigmas0 (typ rhos0 r2) x0 __)
          (app sigmas0 Nil))) sigmas0)
    (\t r1 iHr rs0 rhos0 sigmas0 rho1 x0 _ ->
    eq_rect (typ rhos0 (Abs t r1))
      (unsafeCoerce (\s sigmas1 x1 ->
        two r (typ (Cons t rhos0) r1) (app sigmas0 sigmas1) (App
          (sub (Abs t r1) rs0) s)
          (sub r1 (Mk_sub (Cons s (support rs0)) (shift rs0)))
          (unsafeCoerce iHr (Mk_sub (Cons s (support rs0)) 
            (shift rs0)) (Cons t rhos0) (app sigmas0 sigmas1)
            (typ (Cons t rhos0) r1) (Pair x1
            (sCs_ext_ctx r sigmas0 sigmas1 rhos0 (support rs0) x0)) __)))
      rho1) r0 rs rhos sigmas rho0 x __

sCs_seq :: Requirements -> (List Type) -> (List Type) -> SCs
sCs_seq r rhos sigmas =
  list_rect (unsafeCoerce __) (\a rhos0 iHrhos sigmas0 ->
    unsafeCoerce (Pair
      (case one r a (app sigmas0 (Cons a rhos0)) (Var (length sigmas0)) of
         Pair s s0 -> s0 (\k sigmas1 _ -> Var (length sigmas0)))
      (eq_rect (app (Cons a Nil) rhos0)
        (eq_rect (app (app sigmas0 (Cons a Nil)) rhos0)
          (eq_rect (length (app sigmas0 (Cons a Nil)))
            (iHrhos (app sigmas0 (Cons a Nil))) (S 
            (length sigmas0))) (app sigmas0 (app (Cons a Nil) rhos0))) (Cons
        a rhos0)))) rhos sigmas

normalizeTheorem :: Requirements -> Context -> Type -> Term -> Term
normalizeTheorem r rhos rho0 r0 =
  case one r rho0 rhos r0 of
    Pair s s0 ->
      eq_rec rhos (\h1 -> h1 __) (app rhos Nil)
        (s
          (eq_rect (sub r0 (id (length rhos)))
            (three r r0 (id (length rhos)) rhos rhos rho0
              (sCs_seq r rhos Nil)) r0) (length rhos) Nil)

is_app :: Term -> Bool
is_app r =
  case r of
    Var n -> False
    App t t0 -> True
    Abs t t0 -> False

left_app :: Term -> Term
left_app r =
  case r of
    Var n -> dterm
    App r0 t -> r0
    Abs t t0 -> dterm

right_app :: Term -> Term
right_app r =
  case r of
    Var n -> dterm
    App t r0 -> r0
    Abs t t0 -> dterm

is_eta_core :: Term -> Nat -> Bool
is_eta_core r k =
  andb
    (andb (is_app r)
      (case term_dec (right_app r) (Var k) of
         Left -> True
         Right -> False)) (negb (occurs k (left_app r)))

type SN0 = Nat -> (()->()) -> (()->()) -> Term

type SA0 = Nat -> (()->()) -> (()->()) -> Term

type SC0 = (()->())

sN_ext_ctx0 :: Requirements -> Type -> SN0 -> Nat -> Term
sN_ext_ctx0 r rho0 x k =
  eq_rec_r __ (x k __ __) __

sC_ext_ctx0 :: Requirements -> Type -> SC0 -> SC0
sC_ext_ctx0 r rho0 x =
  case rho0 of
    Iota ->
      halfprod_rect (\_ b ->
        unsafeCoerce (\x0 _ _ -> sN_ext_ctx0 r Iota b x0)) 
        (unsafeCoerce x)
    Arrow rho1 rho2 ->
      halfprod_rect (\_ b ->
        unsafeCoerce (\_ _ x0 ->
          eq_rect_r __ (b __ __ (eq_rect __ x0 __)) __)) 
        (unsafeCoerce x)

one0 :: Requirements -> Type -> Prod (SC0 -> SN0) (SA0 -> SC0)
one0 r rho0 =
  type_rect (\_ _ _ -> Pair (\x ->
    halfprod_rect (\_ b -> b) (unsafeCoerce x)) (\h1 ->
    unsafeCoerce (\k _ _ -> h1 k __ __))) (\rho1 iHrho1 rho2 iHrho2 _ _ _ ->
    Pair (\x k _ _ ->
    abstr r k rho1
      (fst (iHrho2 __ __ __)
        (eq_rect_r __
          (unsafeCoerce x __ __
            (snd (iHrho1 __ __ __) (eq_rect __ (\k0 _ _ -> Var k) __))) __)
        (S k) __ __)) (\h1 ->
    unsafeCoerce (\_ _ x ->
      snd (iHrho2 __ __ __) (\k _ _ -> App (h1 k __ __)
        (fst (iHrho1 __ __ __) x k __ __))))) rho0 __ __ __

two0 :: Requirements -> Type -> SC0 -> SC0
two0 r rho0 x =
  type_rect (\_ _ _ _ h1 _ ->
    unsafeCoerce (\k _ _ -> unsafeCoerce h1 k __ __))
    (\rho1 iHrho1 rho2 iHrho2 _ _ _ _ x0 _ ->
    unsafeCoerce (\_ _ x1 ->
      iHrho2 __ __ __ __
        (halfprod_rect (\_ b -> b __ __ x1) (unsafeCoerce x0)) __)) rho0 __
    __ __ __ x __

type SCs0 = (()->())

sCs_nth0 :: Requirements -> (List Type) -> Type -> Nat -> SCs0 -> SC0
sCs_nth0 r rhos rho0 n x =
  list_rect (\rho1 _ _ n0 _ -> unsafeCoerce (\_ -> false_rect __))
    (\a rhos0 iHrhos rho1 _ _ n0 _ x0 ->
    case unsafeCoerce x0 of
      Pair h s ->
        (case n0 of
           O -> eq_rect_r __ h __
           S n1 -> eq_rect_r __ (iHrhos rho1 __ __ n1 __ s) __)) rhos rho0 __
    __ n __ x

sCs_ext_ctx0 :: Requirements -> Context -> SCs0 -> SCs0
sCs_ext_ctx0 r rhos x =
  list_rect (unsafeCoerce __) (\a rhos0 iHrhos _ x0 ->
    prod_rect (\a0 b ->
      halfprod_rect (\_ b0 ->
        unsafeCoerce (Pair (sC_ext_ctx0 r a b0) (iHrhos __ b))) a0)
      (unsafeCoerce x0)) rhos __ x

three0 :: Requirements -> Term -> Context -> Type -> SCs0 -> SC0
three0 r r0 rhos rho0 x =
  term_rect (\n _ rhos0 _ rho1 x0 _ ->
    eq_rect (nth n rhos0 dtype) (eq_rect __ (sCs_nth0 r rhos0 dtype n x0) __)
      rho1) (\r1 iHr1 r2 iHr2 _ rhos0 _ rho1 x0 _ ->
    eq_rect_r __
      (eq_rect __
        (unsafeCoerce (iHr1 __ rhos0 __ (Arrow (typ rhos0 r2) rho1) x0 __) __
          __ (eq_rect __ (iHr2 __ rhos0 __ (typ rhos0 r2) x0 __) __)) __) __)
    (\t r1 iHr _ rhos0 _ rho1 x0 _ ->
    eq_rect (typ rhos0 (Abs t r1))
      (unsafeCoerce (\_ _ x1 ->
        two0 r (typ (Cons t rhos0) r1)
          (unsafeCoerce iHr __ (Cons t rhos0) __ (typ (Cons t rhos0) r1)
            (Pair (eq_rect __ x1 __)
            (sCs_ext_ctx0 r rhos0 (eq_rect __ x0 __))) __))) rho1) r0 __ rhos
    __ rho0 x __

sCs_seq0 :: Requirements -> (List Type) -> Nat -> SCs0
sCs_seq0 r rhos k =
  list_rect (unsafeCoerce __) (\a rhos0 iHrhos _ k0 _ ->
    unsafeCoerce (Pair
      (case one0 r a of
         Pair s s0 -> s0 (\k1 _ _ -> Var k0))
      (eq_rect __ (iHrhos __ (S k0) __) __))) rhos __ k __

normalizeTheorem0 :: Requirements -> Context -> Type -> Term -> Term
normalizeTheorem0 r rhos rho0 r0 =
  case one0 r rho0 of
    Pair s s0 ->
      s
        (eq_rect (sub r0 (id (length rhos)))
          (eq_rect __ (three0 r r0 rhos rho0 (sCs_seq0 r rhos O)) __) r0)
        (length rhos) __ __

abstr0 :: Nat -> Type -> Term -> Term
abstr0 k rho0 r =
  case is_eta_core r k of
    True -> left_app r
    False -> Abs rho0 (sub_swap0 r k)

requirementsProofEtaRed :: Requirements
requirementsProofEtaRed =
  abstr0

rho :: Type
rho =
  Arrow Iota Iota

quad :: Type -> Type
quad sigma =
  Arrow (Arrow sigma sigma) (Arrow sigma sigma)

f_n_x :: Term -> Term -> Nat -> Term
f_n_x f x n =
  case n of
    O -> x
    S n0 -> App f (f_n_x f x n0)

church :: Nat -> Type -> Term
church n sigma =
  Abs (Arrow sigma sigma) (Abs sigma (f_n_x (Var (S O)) (Var O) n))

church_church :: Nat -> Nat -> Term
church_church n m =
  App (church n (Arrow Iota Iota)) (church m Iota)

ex :: Term
ex =
  normalizeTheorem requirementsProofEtaRed (Cons rho Nil) rho (App (Abs rho
    (Var O)) (Var O))

church_power :: Nat -> Nat -> Term
church_power n m =
  normalizeTheorem requirementsProofEtaRed Nil (quad Iota)
    (church_church n m)


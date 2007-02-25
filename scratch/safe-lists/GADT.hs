{-# OPTIONS_GHC -fglasgow-exts #-}


data ConsT a
data NilT

data List a t where
    Cons :: a -> List a b -> List a (ConsT b)
    Nil  :: List a NilT

instance Show a => Show (List a t) where
    show (Cons a b) = show a ++ ":" ++ show b
    show Nil = "[]"


safeHead :: List a (ConsT t) -> a
safeHead (Cons a b) = a

safeTail :: List a (ConsT t) -> List a t
safeTail (Cons a b) = b


safeMap :: (a -> b) -> List a t -> List b t
safeMap f Nil = Nil
safeMap f (Cons a b) = Cons (f a) (safeMap f b)


-- Using an existential datatype box
data VarList a = forall t. VarList (List a t)

fromListV :: [a] -> VarList a
fromListV []     = VarList Nil
fromListV (x:xs) = case fromListV xs of
                    VarList xs' -> VarList (Cons x xs')

-- using CPS transform (turns existentials into rank2)
-- generally prefered because it avoids naming a one-use data type,
-- but YMMV

fromListC :: [a] -> (forall t. List a t -> r) -> r
fromListC [] fn = fn Nil
fromListC (x:xs) fn = fromListC xs (\sl -> fn (Cons x sl))


main = print "neil"




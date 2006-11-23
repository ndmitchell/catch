
module Data where

-- Just test out data handling capabilites
-- should determine there are no Bar's in the result


data Foo = Bar | Flob | Fido -- Bool
data Group a b = Group a b -- Tup
data Pile a = Zero | One a | Some a (Pile a) -- List



main :: Pile (Group Foo Foo) -> Pile (Group Foo Foo)
main x = f x


f Zero = Zero
f (One a) = Zero
f (Some a b) = Some (g a) (f b)

g (Group a b) = Group (h b) (h a)

h Bar = Flob
h Flob = Fido
h Fido = Flob


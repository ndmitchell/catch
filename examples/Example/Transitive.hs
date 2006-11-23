-- From Matt :)

module Transitive where

main a b c = assert (trans a b c) True

trans a b c = imp ((a `eq` b) && (b `eq` c)) (a `eq` c)

assert True x = x

eq True True = True
eq False False = True
eq _ _ = False

imp True True = True
imp False _ = True
imp _ _ = False

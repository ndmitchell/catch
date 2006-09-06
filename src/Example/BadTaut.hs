-- The bad bit out of Taut

module BadTaut where


data Data = U Bool
          | L {l :: Data}
          | R {r :: Data}
          | F


main x = test x


test (U x) = x
test (L x) = test x
test (R x) = test x


module Referee where


main xs ys = case null xs || null ys of
    True -> []
    False -> [head xs, head ys]




risers3 x y = risers4 (risers (x : y))

risers4 x =
    case x of
        (y:ys) -> (ys, y)
        [] -> error "Risers: No match in pattern expression at 11:12."

risers x =
    case x of
        [] -> []
        (y:ys) ->
            case ys of
                [] -> (y : []) : []
                (z:zs) -> risers2 (risers3 z zs) (y <=# z) y

(<=#) a b = (<=) a b

risers2 x y z =
    case y of
        True -> (z : snd x) : (fst x)
        False -> (z : []) : (snd x : fst x)
        

main v1 = risers v1

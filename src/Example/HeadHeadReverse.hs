
module HeadHeadReverse where


headHead [] = []
headHead (x:xs) = head x

main x = headHead (reverse x)



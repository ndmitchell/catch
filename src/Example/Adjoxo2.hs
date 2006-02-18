
module Adjoxo where

-- Adjudicator for noughts-and-crosses positions
-- See *.in for example inputs.
-- Colin Runciman

data GameValue = Loss | Draw | Win deriving (Eq,Ord,Show)

bestOf :: GameValue -> GameValue -> GameValue 
bestOf Win  v   = Win
bestOf Loss v   = v
bestOf Draw Win = Win
bestOf Draw v   = Draw

inverse :: GameValue -> GameValue
inverse Loss = Win
inverse Draw = Draw
inverse Win  = Loss

-- Positions in the playing grid are numbered 1..9 with
-- top row 1,2,3; middle row 4,5,6; bottom row 7,8,9.
-- Ordered lists of such numbers represent O's or X's.

type Region = [Int]
type Player = Region

insert :: Int -> Region -> Region
insert x []                    = [x]
insert x xs@(y:ys) | x > y     = y : insert x ys
                   | otherwise = x : xs

dif :: Region -> Region -> Region
dif [] ys = []
dif xs [] = xs
dif xs@(x:xs') ys@(y:ys') =
  case compare x y of
  LT -> x : dif xs' ys
  EQ ->     dif xs' ys'
  GT ->     dif xs  ys'

subset :: Region -> Region -> Bool
subset xs ys = null (dif xs ys)

hasLine :: Player -> Bool
hasLine p = subset [1,2,3] p || subset [4,5,6] p || subset [7,8,9] p ||
            subset [1,4,7] p || subset [2,5,8] p || subset [3,6,9] p ||
                      subset [1,5,9] p || subset [3,5,7] p

gridFull :: Player -> Player -> Bool
gridFull ap pp = length ap + length pp == 9

-- The ap argument of analysis is a position list for the active player
-- (to move next); and the pp argument is the list for the passive player.
-- The result represents the outcome with best play on both sides.

analysis :: Player -> Player -> GameValue
analysis ap pp =
  if      hasLine pp then Loss
  else if null free then Draw
  else    foldr1 bestOf (map moveval free)
  where
  moveval m = inverse (analysis pp (insert m ap))
  free = ([1..9] `dif` ap) `dif` pp

-- The argument to parsed is an input string, and its result
-- is the corresponding pair of position-lists for O's and X's.
-- Example:  O | O | 
--          ---+---+---
--             |   |      ==> ([1,2],[7,8])
--          ---+---+---
--           X | X | 

parsed :: String -> (Player, Player)
parsed input = (player 'O' input, player 'X' input)

player :: Char -> String -> Player
player = player' 1

player' :: Int -> Char -> String -> Player
player' _ _ [] = []
player' n p (c:cs) =
  if c=='|' then player' (n+1) p cs
  else if c=='-' then player' (n+1) p (dropWhile (`elem` "-+") cs)
  else if c==p then n : player' n p cs
  else player' n p cs

-- The argument to adjudicate represents positions of O's and X's.
-- Its result is a short adjudicator's report.

adjudicate :: (Player, Player) -> String
adjudicate (os,xs) =
  case compare (length os) (length xs) of
  GT -> report (analysis xs os) 'X'
  EQ -> if hasLine xs then report Win 'X'
        else if hasLine os then report Win 'O'
        else "if X to play: " ++ report (analysis xs os) 'X' ++
             "if O to play: " ++ report (analysis os xs) 'O'
  LT -> report (analysis os xs) 'O'

report :: GameValue -> Char -> String
report Loss p = report Win (opp p) 
report Win  p = "WIN for " ++ [p] ++ "\n"
report Draw _ = "DRAW\n"

opp :: Char -> Char
opp 'O' = 'X'
opp 'X' = 'O'

{-
main =
  do
     input <- getContents
     putStr (adjudicate (parsed input))
-}

main x = adjudicate x

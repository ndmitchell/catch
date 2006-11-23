
-- http://www.haskell.org/pipermail/haskell-cafe/2006-May/015560.html
-- [Haskell-cafe] Are records type safe?
-- Tom Hawkins
-- Thu May 4 09:20:54 EDT 2006


module Records where

data Rec = RecA { a :: Int } | RecB { a :: Int, b :: Bool } deriving Show

main :: IO ()
main = do
 print (b (RecB { a = 1, b = True }))  -- Works
 print (b (RecA { a = 1 }))            -- Doesn't


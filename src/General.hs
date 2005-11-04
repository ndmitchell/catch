
module General where

import Maybe

{-
import IOExts

debugmsg msg val = unsafePerformIO $
                        do putStrLn (show msg)
                           return val

debugmsgres msg val = debugmsg (msg, val) val
-}

a ! b = fromJust $ lookup b a

errorS a = error (show a)

fsts = map fst
snds = map snd

filterFst f = filter (f . fst)
filterSnd f = filter (f . snd)


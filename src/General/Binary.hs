
module General.Binary where

import System.IO
import Data.Char
import Control.Monad
import Data.Predicate


class Binary a where
    put_   :: Handle -> a -> IO ()
    get    :: Handle -> IO a


writeBinary :: Binary a => FilePath -> a -> IO ()
writeBinary file x = do
    hndl <- openBinaryFile file WriteMode
    put_ hndl x
    hClose hndl

readBinary :: Binary a => FilePath -> IO a
readBinary file = do
    hndl <- openBinaryFile file ReadMode
    res <- get hndl
    hClose hndl
    return res


putByte hndl x = hPutChar hndl (chr x)
getByte hndl = liftM ord $ hGetChar hndl


instance Binary a => Binary [a] where
    put_ bh [] = putByte bh 0
    put_ bh xs = do putByte bh (length a); mapM_ (put_ bh) a; put_ bh b
        where (a,b) = splitAt 100 xs
    
    get bh         = do h <- getByte bh
                        case h of
                          0 -> return []
                          _ -> do xs <- replicateM h (get bh)
                                  ys <- get bh
                                  return (xs ++ ys)


instance Binary Char where
    put_ = hPutChar
    get = hGetChar


put_S h x = put_ h (show x)
getS h = liftM read $ get h

instance Binary Int where {put_ = put_S; get = getS}
instance Binary Integer where {put_ = put_S; get = getS}
instance Binary Double where {put_ = put_S; get = getS}
instance Binary Float where {put_ = put_S; get = getS}



instance (Binary a, Binary b) => Binary (a,b) where
    put_ h (a,b) = put_ h a >> put_ h b
    get h = do a <- get h
               b <- get h
               return (a,b)

instance Binary (Pred a) where
    put_ = error "can't put a predicate"
    get = error "can't get a predicate"


module MonadFail2 where

main = putChar 'a' `bind` putChar 'b'


bind :: Monad m => m a -> m b -> m b
bind a b = a >> b

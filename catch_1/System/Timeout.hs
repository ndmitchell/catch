{-# OPTIONS_GHC -cpp #-}

module System.Timeout(timeout) where


{-
#if 0
-}
timeout :: Int -> IO a -> IO (Maybe a)
timeout _ x = x >>= return . Just
{-
#endif
-}


{-
#if 0
{-
#endif
-}
import System.TimeoutGHC
{-
#if 0
-}
#endif
-}

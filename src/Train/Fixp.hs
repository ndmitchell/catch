
module Train.Fixp(fixp) where

import General.General


fixp :: (Output v, Output k, Eq v, Eq k) => v -> (k -> (k -> IO v) -> IO v) -> k -> IO v
fixp def solve key = gen [] key
	where
		-- look up in the stack if possible
		ask stack key = do
			-- putStrLn $ "Searching for: " ++ output key
			-- putStrLn $ "In: " ++ show [(output a, output b) | (a,b) <- stack]
			case lookup key stack of
				Nothing -> {- putStrLn "Not found" >> -} gen stack key
				Just x -> {- putStrLn "Found" >> -} return x
		
		-- create afresh
		gen stack key = f def
			where
				f value = do
					value2 <- solve key (ask ((key,value):stack))
					if value == value2 then return value else f value2

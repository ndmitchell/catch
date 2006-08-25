
module Train.Fixp(fixp) where


fixp :: (Eq v, Eq k) => v -> (k -> (k -> IO v) -> IO v) -> k -> IO v
fixp def solve key = gen [] key
	where
		-- look up in the stack if possible
		ask stack key = case lookup key stack of
							Nothing -> gen stack key
							Just x -> return x
		
		-- create afresh
		gen stack key = f def
			where
				f value = do
					value2 <- solve key (ask ((key,value):stack))
					if value == value2 then return value else f value2

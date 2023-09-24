mapping :: (a -> b) -> [a] -> [b]
mapping _ [] = []
mapping f (h:t) = f h : mapping f t
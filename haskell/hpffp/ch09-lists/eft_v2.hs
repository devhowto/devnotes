{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using cons.
-- Getting a pattern not matched: _ _
--
eft :: (Ord a, Enum a) => a -> a -> [a]
eft ini end
  | ini > end = []
  | ini == end = [ini]
  | ini < end = ini : eft (succ ini) end


--
-- We use l as an accumulator and make this implementation
-- tail recursive.
--
eFt :: (Ord a, Enum a) => a -> a -> [a]
eFt ini end = go ini end []
  where
    go :: (Ord a, Enum a) => a -> a -> [] a -> [] a
    go i e l
      | i > e = []
      | i == e = [i]
      | otherwise = go (succ i) e (l ++ [i])

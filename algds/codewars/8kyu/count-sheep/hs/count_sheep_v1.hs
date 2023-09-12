--
-- tags: filter count
--

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Manual recursion. fromEnum returns 0 for False and 1 for True, so
-- we always add either 0 or 1 to the cnt accumulator.
--
countSheep :: [Bool] -> Int
countSheep bools = go bools 0
  where go :: [Bool] -> Int -> Int
        go [] cnt       = cnt
        go (b : bs) cnt = go bs (fromEnum b + cnt)

run :: IO ()
run = do
  print $ countSheep []
  print $ countSheep [False, False, True, False]
  print $ countSheep [True, True]

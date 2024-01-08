--
-- tags: filter count
--

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Manual recursion. Only increment cnt if the current boolean is True.
--
countSheep :: [Bool] -> Int
countSheep bools = go bools 0
  where go :: [Bool] -> Int -> Int
        go [] cnt       = cnt
        go (b : bs) cnt =
          case b == True of
            True -> go bs (succ cnt)
            _    -> go bs cnt

run :: IO ()
run = do
  print $ countSheep []
  print $ countSheep [False, False, True, False]
  print $ countSheep [True, True]

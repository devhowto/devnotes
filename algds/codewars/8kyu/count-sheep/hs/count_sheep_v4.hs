--
-- tags: filter count
--

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Function free, function composition.
--
-- Because filter keeps elements to which the predicate returns True,
-- by simply using id, it will already return True of False. We don't
-- even need to compare (== True) like in the previous solution.
--
countSheep :: [Bool] -> Int
countSheep  = length . filter id

run :: IO ()
run = do
  print $ countSheep []
  print $ countSheep [False, False, True, False]
  print $ countSheep [True, True]

--
-- tags: filter count
--

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Function free, function composition.
--
countSheep :: [Bool] -> Int
countSheep  = length . filter (== True)

run :: IO ()
run = do
  print $ countSheep []
  print $ countSheep [False, False, True, False]
  print $ countSheep [True, True]

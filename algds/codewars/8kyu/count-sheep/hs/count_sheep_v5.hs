--
-- tags: filter count
--

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Function free, function composition.
--
-- Using list comprehension and sum stdfn.
--
countSheep :: [Bool] -> Int
countSheep lob = sum [1 | x <- lob, x == True]
-- λ> lob = [True, False, False, True, True, False, True]
-- λ> [1 | b <- lob, b == True]
-- [1,1,1,1]
-- λ> sum it
-- 4

run :: IO ()
run = do
  print $ countSheep []
  print $ countSheep [False, False, True, False]
  print $ countSheep [True, True]

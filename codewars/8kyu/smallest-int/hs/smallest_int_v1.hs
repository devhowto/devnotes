{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using guards and recursion.
--
findSmallest :: [] Int -> Int
findSmallest ints = go ints (head ints)
  where
    go :: [Int] -> Int -> Int
    go xs minSoFar
      | null xs = minSoFar
      | (head xs) < minSoFar = go (tail xs) (head xs)
      | otherwise = go (tail xs) minSoFar

run :: IO ()
run = do
  putStrLn $ show $ findSmallest [5, 1, 3]
  putStrLn $ show $ findSmallest [-7, 1, -9]
  putStrLn $ show $ findSmallest [42, 42, 42]

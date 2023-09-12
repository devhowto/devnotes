{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- List comprehension.
--
positiveSum :: [Int] -> Int
positiveSum xs = sum [x | x <- xs, x > 1]

run :: IO ()
run = do
  putStrLn $ show $ positiveSum []
  putStrLn $ show $ positiveSum [-1, -5, -3]
  putStrLn $ show $ positiveSum [-1, 1, -3, 41]

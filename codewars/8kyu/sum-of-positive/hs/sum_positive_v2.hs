{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- We'll apply + to either a value with is greater than 0,
-- or 0. The partial application (max 0) makes sure we default
-- to 0 if the current int is less than 0.
--
positiveSum :: [Int] -> Int
positiveSum = foldr ((+) . (max 0)) 0

run :: IO ()
run = do
  putStrLn $ show $ positiveSum []
  putStrLn $ show $ positiveSum [-1, -5, -3]
  putStrLn $ show $ positiveSum [-1, 1, -3, 41]

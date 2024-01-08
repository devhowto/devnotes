{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Function composition and sectioning.
--
-- Sum only > 0 instead of >= 0 because adding 0 won't change
-- the result anyway.
--
positiveSum :: [Int] -> Int
positiveSum = sum . filter (> 0)

run :: IO ()
run = do
  putStrLn $ show $ positiveSum []
  putStrLn $ show $ positiveSum [-1, -5, -3]
  putStrLn $ show $ positiveSum [-1, 1, -3, 41]

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- foldl just for kicks. Also composition and sectioning.
--
positiveSum :: [Int] -> Int
positiveSum = foldl (+) 0 . filter (> 0)

run :: IO ()
run = do
  putStrLn $ show $ positiveSum []
  putStrLn $ show $ positiveSum [-1, -5, -3]
  putStrLn $ show $ positiveSum [-1, 1, -3, 41]

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using guards.
--
makeNegative :: (Ord a, Num a) => a -> a
makeNegative n
  | n > 0 = (- n)
  | otherwise = n

run :: IO ()
run = do
  putStrLn $ show $ makeNegative (-1)
  putStrLn $ show $ makeNegative (42)
  putStrLn $ show $ makeNegative 0

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Function composition and point-free.
--
makeNegative :: (Ord a, Num a) => a -> a
makeNegative = negate . abs

run :: IO ()
run = do
  putStrLn $ show $ makeNegative (-1)
  putStrLn $ show $ makeNegative (42)
  putStrLn $ show $ makeNegative 0

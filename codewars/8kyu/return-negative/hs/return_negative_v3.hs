{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Look at how we use - here!
--
makeNegative :: (Ord a, Num a) => a -> a
makeNegative x = - abs x
-- makeNegative x = (negate . abs) x

{-
min (- x) x
-}


run :: IO ()
run = do
  putStrLn $ show $ makeNegative (-1)
  putStrLn $ show $ makeNegative (42)
  putStrLn $ show $ makeNegative 0

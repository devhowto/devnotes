{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- People sometimes use (-1) to negate a value, but it looks like
-- “nevative 1”. Because ‘-’ is an alias for ‘negate’, and we wouldn't
-- write (negate1), but (negate 1), we should write (- 1) with a space
-- when we are using ‘-’ to mean ‘negate’ instead of ‘negative’.
--
invert :: [Integer] -> [Integer]
invert xs = [(- x) | x <- xs]

run :: IO ()
run = do
  putStrLn $ show $ invert [1..4]
  putStrLn $ show $ invert $ enumFromThenTo (-1) (-2) (-4)
  putStrLn $ show $ invert [-1, 2, -3, 4, 0]

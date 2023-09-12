{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using sectioning to subtract 0 from x. Point-free.
--
invert :: [Integer] -> [Integer]
invert = map (0 -)

run :: IO ()
run = do
  putStrLn $ show $ invert [1..4]
  putStrLn $ show $ invert $ enumFromThenTo (-1) (-2) (-4)
  putStrLn $ show $ invert [-1, 2, -3, 4, 0]

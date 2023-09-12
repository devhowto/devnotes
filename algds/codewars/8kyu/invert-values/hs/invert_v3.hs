{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using sectioning to multiply some x by -1. Point-free.
--
invert :: [Integer] -> [Integer]
invert = map (* (-1))

run :: IO ()
run = do
  putStrLn $ show $ invert [1..4]
  putStrLn $ show $ invert $ enumFromThenTo (-1) (-2) (-4)
  putStrLn $ show $ invert [-1, 2, -3, 4]

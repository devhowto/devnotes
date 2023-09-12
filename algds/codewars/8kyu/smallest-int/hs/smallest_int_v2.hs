{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using guards and recursion.
--
findSmallest :: [] Int -> Int
findSmallest [] = error "Empty list has no smallest."
findSmallest [x] = x
findSmallest (x : y : rest) =
  if x < y
  then findSmallest (x : rest)
  else findSmallest (y : rest)

run :: IO ()
run = do
  putStrLn $ show $ findSmallest [5, 1, 3]
  putStrLn $ show $ findSmallest [-7, 1, -9]
  putStrLn $ show $ findSmallest [42, 42, 42]

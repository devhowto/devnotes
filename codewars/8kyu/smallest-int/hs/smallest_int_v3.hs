{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using guards and recursion.
--
findSmallest :: [] Int -> Int
findSmallest [] = error "Empty list has no smallest."
findSmallest xs = foldr1 min xs

run :: IO ()
run = do
  putStrLn $ show $ findSmallest [5, 1, 3]
  putStrLn $ show $ findSmallest [-7, 1, -9]
  putStrLn $ show $ findSmallest [42, 42, 42]

{-

The challenge mentions we can assume a non-empty list. If so, we could
make it point-free:

    findSmallest = foldr1 min

    f :: [Int] -> Int
    f = foldr1 min
-}

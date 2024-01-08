{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using guards and built-in even check.
--
evenOrOdd :: Integral a => a -> String
evenOrOdd n
  | even n    = "Even"
  | otherwise = "Odd"

run :: IO ()
run = do
  putStrLn $ evenOrOdd (-1)
  putStrLn $ evenOrOdd (3)
  putStrLn $ evenOrOdd (4)
  putStrLn $ evenOrOdd (-14)
  putStrLn $ evenOrOdd (0)
  putStrLn $ evenOrOdd (-0)

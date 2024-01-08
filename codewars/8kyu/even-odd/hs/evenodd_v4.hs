{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Pattern-matching and a recursive approach (even though this
-- will recur at most once :)).
--
evenOrOdd :: Integral a => a -> String
evenOrOdd 0    = "Even"
evenOrOdd 1    = "Odd"
evenOrOdd (-1) = "Odd"
evenOrOdd n    = evenOrOdd (rem n 2)

run :: IO ()
run = do
  putStrLn $ evenOrOdd (-1)
  putStrLn $ evenOrOdd (3)
  putStrLn $ evenOrOdd (4)
  putStrLn $ evenOrOdd (-14)
  putStrLn $ evenOrOdd (0)
  putStrLn $ evenOrOdd (-0)

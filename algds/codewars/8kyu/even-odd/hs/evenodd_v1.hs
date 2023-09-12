{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using guards. Doing manual even check just for kicks.
--
evenOrOdd :: Integral a => a -> String
evenOrOdd n
  | rem n 2 == 0 = "Even"
  | otherwise    = "Odd"

run :: IO ()
run = do
  putStrLn $ evenOrOdd (-1)
  putStrLn $ evenOrOdd (3)
  putStrLn $ evenOrOdd (4)
  putStrLn $ evenOrOdd (-14)
  putStrLn $ evenOrOdd (0)
  putStrLn $ evenOrOdd (-0)

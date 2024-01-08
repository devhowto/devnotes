{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using case/of. Doing manual even check just for kicks.
--
evenOrOdd :: Integral a => a -> String
evenOrOdd n = s
  where s :: String
        s = case rem n 2 == 0 of
          True  -> "Even"
          False -> "Odd"

run :: IO ()
run = do
  putStrLn $ evenOrOdd (-1)
  putStrLn $ evenOrOdd (3)
  putStrLn $ evenOrOdd (4)
  putStrLn $ evenOrOdd (-14)
  putStrLn $ evenOrOdd (0)
  putStrLn $ evenOrOdd (-0)

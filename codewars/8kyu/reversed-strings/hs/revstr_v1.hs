{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using manual recursion. Simply using ‘reverse’
-- would not be fun 😅.
--
rev :: [Char] -> [Char]
rev []     = []
rev (c:cs) = rev cs ++ [c]

run :: IO ()
run = do
  putStrLn $ show $ rev "hello"
  putStrLn $ show $ rev "world"
  putStrLn $ show $ rev "ana"

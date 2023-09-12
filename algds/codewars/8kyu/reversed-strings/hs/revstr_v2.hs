{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using manual recursion. Simply using ‘reverse’
-- would not be fun 😅.
--
-- go is ail-recursive.
--
rev :: [Char] -> [Char]
rev chars = go chars []
  where go :: [Char] -> [Char] -> [Char]
        go [] acc     = acc
        go (c:cs) acc = go cs ([c] ++ acc)

run :: IO ()
run = do
  putStrLn $ show $ rev "hello"
  putStrLn $ show $ rev "world"
  putStrLn $ show $ rev "ana"

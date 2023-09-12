{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Everything here looks “reverse” 🤣.
--
rev :: [Char] -> [Char]
rev (t:h) = rev h ++ t : []
rev []    = []

run :: IO ()
run = do
  putStrLn $ show $ rev "hello"
  putStrLn $ show $ rev "world"
  putStrLn $ show $ rev "ana"

{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Manual tail recursive using null, head and tail :).
--
noSpace :: String -> String
noSpace str = go str ""
  where go :: String -> String -> String
        go s acc
          | null s = acc
          | head s == ' ' = go (tail s) acc
          | otherwise = go (tail s) (acc ++ [head s])


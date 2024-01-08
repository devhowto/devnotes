{-# LANGUAGE NoMonomorphismRestriction #-}

-- noSpace :: String -> String
-- noSpace ""         = ""
-- noSpace (' ' : cs) = noSpace cs
-- noSpace (c : cs)   = c : noSpace cs

--
-- Pattern matching and consing.
--
noSpace :: [Char] -> [Char]
noSpace []         = []
noSpace (' ' : cs) = noSpace cs
noSpace (c : cs)   = c : noSpace cs

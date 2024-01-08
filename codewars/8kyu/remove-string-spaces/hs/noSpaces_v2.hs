{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Very nice solution using filter. Point-free, partial
-- application and sectioning.
--
noSpace :: String -> String
noSpace = filter (/= ' ')

{-

Could also use Data.Char (isSpace) then

    filter (not . isSpace)

Another simple approach:

    concat . words

Or

   s = " k  1  z "
   [c | c <- s, c /= ' ']
-}

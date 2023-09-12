{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- ASSUME: The string contains at least two chars.
--
remfl :: String -> String
remfl s = drop 1 $ take (length s - 1) s

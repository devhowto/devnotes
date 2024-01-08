{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Pattern matching, function composition.
--
remfl :: String -> String
remfl []     = []
remfl (h:[]) = [h]
remfl s      = tail . init $ s

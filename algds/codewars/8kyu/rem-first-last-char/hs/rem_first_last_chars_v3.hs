{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Pattern matching.
--
remfl :: String -> String
remfl []     = []
remfl (h:[]) = [h]
remfl (_:t)  = init t
-- λ> remfl ""
-- ""
-- λ> remfl "z"
-- "z"
-- λ> remfl "zy"
-- ""
-- λ> remfl "Yoda"
-- "od"

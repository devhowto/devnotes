{-# LANGUAGE NoMonomorphismRestriction #-}

import           Prelude (Int, String, (>>))

repeatStr :: Int -> String -> String
repeatStr n s = [1 .. n] >> s
-- λ> repeatStr 3 "ha"
-- "hahaha"
-- λ> repeatStr 2 "hi"
-- "hihi"

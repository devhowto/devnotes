{-# LANGUAGE NoMonomorphismRestriction #-}

import           Prelude (Int, String, pred, (++))

repeatStr :: Int -> String -> String
repeatStr 0 _ = ""
repeatStr n s = (++) s (repeatStr (pred n) s)

--
-- ‘repeat’ can (and likely should) be type-agnostic
-- as it doesn't really care about the types of then
-- list constituents.
--
repeat :: Int -> [a] -> [a]
repeat 0 _ = []
repeat n s = (++) s (repeat (pred n) s)
-- λ> repeat 0 "1"
-- ""
-- λ> repeat 3 ""
-- ""
-- λ> repeat 3 "1"
-- "111"
-- λ> repeat 11 "1"
-- "11111111111"

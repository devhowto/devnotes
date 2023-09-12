{-# LANGUAGE NoMonomorphismRestriction #-}

import           Prelude (Int, String, concat, replicate, (.))

repeatStr :: Int -> String -> String
repeatStr n = concat . replicate n
-- λ> repeatStr 3 "ha"
-- "hahaha"
-- λ> repeatStr 2 "hi"
-- "hihi"

{-

Partially apply ‘replicate’, then compose it with ‘concat’. The result
is a function that is partially point free. ‘n’ is necessary to
partially apply to ‘replicate’, but the string itself can be omitted
from the explicit param declaration.

-}

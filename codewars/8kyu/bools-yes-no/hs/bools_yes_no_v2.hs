{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Bool (bool)

--
-- Partially apply bool to "No" and "Yes" and waits for the last
-- argument to return the final result.
--
boolToWord :: Bool -> String
boolToWord = bool "No" "Yes"

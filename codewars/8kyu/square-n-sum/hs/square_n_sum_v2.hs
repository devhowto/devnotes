{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Using more manual recursion approach.
--
squareSum :: [Integer] -> Integer
squareSum []       = 0
squareSum (x : xs) = x * x + squareSum xs

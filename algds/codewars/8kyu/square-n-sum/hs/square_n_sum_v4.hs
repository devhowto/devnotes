{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Function composition and sectioned application of (^).
-- Also point-free style.
--
squareSum :: [Integer] -> Integer
squareSum = foldr ((+) . (^ 2)) 0

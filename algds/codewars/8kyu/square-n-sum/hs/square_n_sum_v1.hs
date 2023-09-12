{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Square x using multiplication and then sum using stdfn.
--
squareSum :: [Integer] -> Integer
squareSum xs = sum $ map (\x -> x * x) xs

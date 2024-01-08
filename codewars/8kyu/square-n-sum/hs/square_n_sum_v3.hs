{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Function composition and sectioned application of (^).
-- Also point-free style.
--
squareSum :: [Integer] -> Integer
squareSum = sum . map (^ 2)

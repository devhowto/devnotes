{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Point-free and sectioning subtraction.
--
opposite :: Num a => a -> a
opposite = (0 -)

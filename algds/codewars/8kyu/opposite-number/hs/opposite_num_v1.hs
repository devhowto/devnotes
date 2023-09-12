{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Point-free.
--
opposite :: Num a => a -> a
opposite = negate

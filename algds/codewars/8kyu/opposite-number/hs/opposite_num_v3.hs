{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Point-free and sectioning multiplication.
--
opposite :: Num a => a -> a
opposite = ((-1) *)

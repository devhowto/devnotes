{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Solution is just like ‘negate n’ as ‘-’ is an alias to ‘negate’.
--
opposite :: Num a => a -> a
opposite n = - n

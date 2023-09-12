{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- What about this‽
--
opposite :: Num a => a -> a
opposite n = n - n - n

{-

Could also do ‘n - (2 * n)’.

-}

module E05a where

--
-- From the minuend, subtract the subtrahend.
--
--  minuend - subtrahend = difference
--
sub :: Integer -> Integer -> Integer
sub minuend subtrahend = (-) minuend subtrahend

subFrom5 :: Integer -> Integer
subFrom5 = sub 5

overTwo :: Fractional a => a -> a
overTwo = (/ 2)

twoOver :: Fractional a => a -> a
twoOver = (2 /)


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

sub1 :: Integer -> Integer
sub1 = (subtract 1)

sub10 :: Integer -> Integer
sub10 = ((-) 10)

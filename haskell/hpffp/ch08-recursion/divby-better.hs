{-# LANGUAGE NoMonomorphismRestriction #-}

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

data Division =
    Result (Quotient, Remainder)
  | DividedByZero
  deriving Show

--
-- `divBy` is my implementation of `quotRem` in terms of subtraction.
--
-- Perform the subtraction on the absolute values and then negate the
-- tuple constituents according to the division rules.
--
divBy :: Numerator -> Denominator -> Division
divBy _   0     = DividedByZero
divBy num denom = signify $ go (abs num) (abs denom) 0
  where
    go :: Numerator -> Denominator -> Quotient -> Division
    go n d count
      | d == 0    = DividedByZero
      | n < d     = Result (count, n)
      | otherwise = go (n - d) d (count + 1)
    signify :: Division -> Division
    signify DividedByZero = DividedByZero
    signify (Result (q, r))
      | num < 0 && denom < 0 = Result (q, (- r))
      | num < 0              = Result ((- q), (- r))
      | denom < 0            = Result ((- q), r)
      | otherwise            = Result (q, r)
--
-- λ> quotRem (-7) (-2)
-- (3,-1)
-- λ> divBy (-7) (-2)
-- Result (3,-1)
--
-- λ> quotRem 7 2
-- (3,1)
-- λ> divBy 7 2
-- Result (3,1)
--
-- λ> quotRem (-7) 2
-- (-3,-1)
-- λ> divBy (-7) 2
-- Result (-3,-1)
--
-- λ> quotRem 7 (-2)
-- (-3,1)
-- λ> divBy 7 (-2)
-- Result (-3,1)
--

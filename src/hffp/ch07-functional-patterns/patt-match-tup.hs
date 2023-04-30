-- {-# LANGUAGE NoMonomorphismRestriction #-}

module TupleFns where

-- (+) is a -> a -> a, so x and y must be of the same type.
-- We are requiring a tuple, but it cannot be of (a, b), it
-- has to be (a, a) because of (+).
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = (+) x y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, z) = z

k (x, _) = x

k1 = k ((4 - 1), 10)

k2 = k ("three", (1 + 2))

k3 = k (3, True)

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))


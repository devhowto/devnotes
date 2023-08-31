{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Fibonacci uses positive integers.
--
-- This version does not use memoization or dynamic programming in
-- anyway to remember previous computations of a fib number therefore
-- optimizing the execution time. This solution looks simple enough, but
-- it is far from having any godd performance at all. fib 35 takes about
-- 10 senconds on a AMD Ryzen 9 3900X 12-Core processor for example.
--
fib :: Word -> Word
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

{-

fib 7 = (fib 7) + (fib 6)
fib 6 = (fib 6) + (fib 5)
fib 5 = (fib 4) + (fib 3)
fib 4 = (fib 3) + (fib 2)
fib 3 = (fib 2) + (fib 1)
fib 2 = (fib 1) + (fib 0)

fib 1 = 1
fib 0 = 0
fib 2 = 1 + 0 = 1
fib 3 = 1 + 1 = 2
fib 4 = 2 + 1 = 3
fib 5 = 3 + 2 = 5
fib 6 = 5 + 3 = 8
fib 7 = 8 + 5 = 13

-}

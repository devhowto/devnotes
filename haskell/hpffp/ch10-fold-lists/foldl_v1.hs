{-# LANGUAGE NoMonomorphismRestriction #-}

fldr :: (a -> b -> b) -> b -> [a] -> b
fldr _ acc []       = acc
fldr f acc (x : xs) = f x (fldr f acc xs)

fldl :: (b -> a -> b) -> b -> [a] -> b
fldl _ acc []       = acc
fldl f acc (x : xs) = fldl f (f acc x) xs

{-

f = (+)

fldl f 0 [1, 2, 3]
fldl f (f 0 1) [2, 3]
        + 0 1

fldl f (f 1 2) [3]
        + 1 2

fldl f (f 3 3) []

dbg x y = concat ["(", x, " + ", y, ")"]

-}


dbg :: [Char] -> [Char] -> [Char]
dbg x y = concat ["(", x, " + ", y, ")"]
--
-- λ> fldl dbg "0" (map show [1 .. 3])
-- "(((0 + 1) + 2) + 3)"
--

{-

f = (-)

fldr f 0 [1, 2, 3]

- 1 (fldr - 0 [2, 3])

- 1 (- 2 (fldr - 0 [3]))

- 1 (- 2 (- 3 (fldr - 0 []))

- 1 (- 2 (- 3 0))

- 1 (- 2 3)

- 1 (-1)

2


fldl - 0 [1, 2, 3]

fldl - (- 0 1) [2, 3]

fldl - (- -1 2) [3]

fldl - (- -3 3) []

-6 (base case)

-}

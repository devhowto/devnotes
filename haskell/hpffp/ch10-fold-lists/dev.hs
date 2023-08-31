{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

myFoldR :: (a -> b -> b) -> b -> [a] -> b
myFoldR = foldr


fldr :: (a -> b -> b) -> b -> [a] -> b
fldr _ z []       = z
fldr f z (x : xs) = f x (fldr f z xs)

fldl :: (b -> a -> b) -> b -> [a] -> b
fldl _ z []       = z
fldl f z (x : xs) = fldl f (f z x) xs

{-
f = (+)

fldl f 0 [1, 2, 3]
fldl f (f 0 1) [2, 3]
fldl f ((f 0 1)
-}


--
-- A “print fold right” utility to display the folding right
-- of simple lists of numbers using the `+` operator.
--
pfr :: Foldable t => t [Char] -> [Char]
pfr = foldr (\x y -> concat ["(", x, " + ", y, ")"]) "0"
--
-- λ> pfr $ map show [1 .. 5]
-- "(1 + (2 + (3 + (4 + (5 + 0)))))"
--

--
-- Takes a predicate `f` and a list of bools `lob` and returns `True`
-- if any of the bools is `True`; `False` otherwise.
--
myAny :: (a -> Bool) -> [a] -> Bool
myAny f lob =
  foldr (\x b -> f x || b) False lob

{-
myAny even [1, 3, 4, 5]

-}

pfl :: [Char] -> [Char] -> [Char]
pfl x y = concat ["(", x, " + ", y, ")"]
--
-- λ> foldl pfl "0" $ map show [1 .. 5]
-- "(((((0 + 1) + 2) + 3) + 4) + 5)"
--

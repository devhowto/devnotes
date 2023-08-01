{-# LANGUAGE NoMonomorphismRestriction #-}

{-
myAnd :: [Bool] -> Bool
myAnd []       = True
myAnd (b : bs) = case b of
  False -> False
  _     -> myAnd bs

myAnd :: [Bool] -> Bool
myAnd []       = True
myAnd (b : bs) = if b then myAnd bs else False

-}

myAnd :: [Bool] -> Bool
myAnd []       = True
myAnd (b : bs) = (&&) b (myAnd bs)
-- myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr []       = False
myOr (b : bs) = b || myOr bs
-- myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []       = False
myAny f (x : xs) = f x || myAny f xs
--
-- λ> myAny even [1, 3, 4, 5]
-- True
--

-- myElem :: Eq a => a -> [a] -> Bool
-- myElem _ []       = False
-- myElem e (x : xs) = (==) e x || myElem e xs
--
-- λ> myElem 3 [7, 9, 3, 1, 3]
-- True
--

myElem :: Eq a => a -> [a] -> Bool
myElem e = myAny (e ==)
--
-- λ> myElem 3 [1, 5, 9]
-- False
--
-- λ> myElem 3 [1, 5, 3, 9]
-- True
--

myRev :: [a] -> [a]
myRev []       = []
myRev (x : xs) = myRev xs ++ [x]
--
-- λ> myRev "xyz"
-- "zyx"
--

squish :: [[a]] -> [a]
squish []         = []
squish (xs : xss) = xs ++ squish xss
-- squish :: [[a]] -> [a]
-- squish = foldr (++) []
--
-- λ> squish [[1], [2], [3]]
-- [1,2,3]
--

squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap _ []         = []
-- squishMap f (xs : xss) = f xs ++ squishMap f xss
squishMap f = squish . map f
--
-- λ> squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
--
-- λ> squishMap (\x -> [x + 1]) [1, 2, 3]
-- [2,3,4]
--

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
--
-- λ> squishAgain [[1], [2], [3]]
-- [1,2,3]
--

{-
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = go f (tail xs) (head xs)
  where
    go :: (a -> a -> Ordering) -> [a] -> a -> a
    go _  []             winnerSoFar = winnerSoFar
    go fn (first : rest) winnerSoFar =
      if fn first winnerSoFar == GT
      then go fn rest first
      else go fn rest winnerSoFar

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = go f (tail xs) (head xs)
  where
    go :: (a -> a -> Ordering) -> [a] -> a -> a
    go _  []        winner = winner
    go fn (h : lst) winner =
      case fn h winner of
        GT -> go fn lst h
        _  -> go fn lst winner
-}
-- myMaximumBy _ []       = error "myMaximumBy is not defined for empty lists."

--
-- https://discord.com/channels/280033776820813825/505367988166197268/1135899035044151366
--
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x]      = x
myMaximumBy f (x : xs) =
  case f x (myMaximumBy f xs) of
    LT -> myMaximumBy f xs
    _  -> x
  where g = myMaximumBy f xs
--
-- λ> myMaximumBy compare [1, 5, 3]
-- 5
--

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

-- mySquish :: [[a]] -> [a]
-- mySquish []         = []
-- mySquish (xs : xss) = xs ++ mySquish xss
squish :: [[a]] -> [a]
squish = foldr (++) []
--
-- λ> mySquish [[1], [2], [3]]
-- [1,2,3]
--

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (xs : xss) = f xs ++ squishMap f xss
--
-- λ> squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
--
-- λ> squishMap (\x -> [x + 1]) [1, 2, 3]
-- [2,3,4]
--


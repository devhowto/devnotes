{-# LANGUAGE MonomorphismRestriction #-}

module Dev where

eftBool :: Bool -> Bool -> [] Bool
eftBool False False = [False]
eftBool False True  = [False, True]
eftBool True True   = [True]
eftBool True _      = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT GT = [GT]
eftOrd GT _  = []
eftOrd EQ _  = []

--
-- Tail-recursive solution.
--
eftInt :: Int -> Int -> [Int]
eftInt start stop = go start stop []
  where go :: Int -> Int -> [Int] -> [Int]
        go n m l
          | n > m = []
          | n == m = l ++ [n]
          | otherwise = go (succ n) m (l ++ [n])

--
-- Consing, not tail-recursive.
--
eftI :: Int -> Int -> [Int]
eftI start stop = go start stop []
  where go :: Int -> Int -> [] Int -> [] Int
        go ini end lst
          | ini > end = []
          | ini == end = (:) ini lst
          | otherwise = (:) ini (go (succ ini) end lst)

eftC :: Char -> Char -> [Char]
eftC start stop = go start stop []
  where go :: Char -> Char -> [] Char -> [] Char
        go ini end lst
          | ini > end = []
          | ini == end = lst ++ [ini]
          | otherwise = go (succ ini) end (lst ++ [ini])

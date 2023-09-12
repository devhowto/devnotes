{-# LANGUAGE RankNTypes, LambdaCase #-}

--
-- Some more different ways of doing it.
--

import Data.List (sortBy)

xs :: [Int]
xs = [5, 100, 27, 42, -45, 3]

f :: Char -> [Int] -> [Int]
f c = sortBy cmp
  where cmp :: Ord a => a -> a -> Ordering
        cmp = case c == 'L' of
          True  -> flip compare
          False -> compare

--
-- Needs Rankntypes.
--
g :: Char -> [Int] -> [Int]
g c = sortBy (cmp c)
  where cmp :: Char -> Ord a => a -> a -> Ordering
        cmp v
          | v == 'L'  = flip compare
          | v == 'R'  = compare
          | otherwise = error "Neither L or R"

--
-- Needs Lambdacase.
--
h :: Char -> [Int] -> [Int]
h = sortBy . \case 'L' -> flip compare; _ -> compare

--
-- Still needs the semicolon.
--
q :: Char -> [Int] -> [Int]
q = sortBy . \case
  'L' -> flip compare;
    _ -> compare


--
-- Version 1. Verbose, naive approach.
--
module GravityFlip (gravityFlip) where

import Data.List (sortBy)

asc :: Int -> Int -> Ordering
asc x y =
  case compare x y of
    LT -> LT
    GT -> GT
    EQ -> EQ

desc :: Int -> Int -> Ordering
desc x y =
  case compare x y of
    LT -> GT
    GT -> LT
    EQ -> EQ

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip 'R' xs = sortBy asc xs
gravityFlip 'L' xs = sortBy desc xs
gravityFlip _ _ = error "w00t‽"


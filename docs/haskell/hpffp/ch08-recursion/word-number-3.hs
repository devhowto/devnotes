{-# LANGUAGE NoMonomorphismRestriction #-}

module WordNumbers where

import           Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "not 0..9"

digits :: Int -> [Int]
digits n = go n []
  where go :: Int -> [Int] -> [Int]
        go x xs
          | x < 10 = [x] ++ xs
          | otherwise = go (div x 10) ((:) (mod x 10) xs)

wordNum :: Int -> String
wordNum n = concat . intersperse "-" . map digitToWord $ digits n
-- λ> wordNum 1984
-- "one-nine-eight-four"

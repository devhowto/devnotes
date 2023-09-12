--
-- Version 3.
--
-- Point-free style.
--
module GravityFlip (gravityFlip) where

import Data.List (sortBy)

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip 'R' = sortBy compare
gravityFlip 'L' = sortBy (flip compare)
gravityFlip _ = error "w00t‽"

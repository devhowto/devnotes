--
-- Version 4.
--
-- Point-free and function composition, using vanilla sort function.
--
-- By first reversing then sorting, this solution is likely not very
-- performant.
--
module GravityFlip (gravityFlip) where

import Data.List (sort)

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip 'R' = sort
gravityFlip 'L' = reverse . sort
gravityFlip _ = error "w00t‽"


--
-- Version 2.
--
-- compare does ascending order comparison by default. If we simply
-- flip its params, we get a descending order comparison!
--
module GravityFlip (gravityFlip) where

import Data.List (sortBy)

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip 'R' xs = sortBy compare xs
gravityFlip 'L' xs = sortBy (flip compare) xs
gravityFlip _ _ = error "w00t‽"


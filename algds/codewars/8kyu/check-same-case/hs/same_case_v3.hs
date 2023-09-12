{-# LANGUAGE NoMonomorphismRestriction #-}

module CheckSameCase (sameCase, run) where

import Data.Char (isLetter, isLower)

--
-- ASSUME: English Alphabet
--
sameCase :: Char -> Char -> Int
sameCase c1 c2
  | all isLetter [c1, c2] = fromEnum (isLower c1 == isLower c2)
  | otherwise = -1

--
-- NOTE: Solution based on:
--
-- • https://www.codewars.com/kata/reviews/62556092c37be10001eadec9/groups/625855fbba6c6c000126f5d3
--
-- If both chars are letters, then check if the have the same
-- case by the clever `isLower c1 == isLower c2` logic.
--
-- We could have used isUpper and the results would
-- be correct just the same.
--
-- If both isLower is True, True == True is True.
-- If both are NOT isLower, False == False is also True.
-- If any has a different case, we'll end up with something
-- like True == False which is False.
--
-- Finally, fromEnum True is 1, and fromEnum False is 0. There's
-- the solution. The first guard returns either 0 or 1. -1 is
-- returned in case not both chars are letters at the end.
--

run :: IO ()
run = do
  putStrLn (show $ sameCase 'z' '@')
  putStrLn (show $ sameCase 'p' 'Q')
  putStrLn (show $ sameCase 'z' 'k')
  putStrLn (show $ sameCase 'K' 'Z')


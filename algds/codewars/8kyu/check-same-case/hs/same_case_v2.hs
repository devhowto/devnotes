{-# LANGUAGE NoMonomorphismRestriction #-}

module CheckSameCase (sameCase, run) where

import Data.Char (isLower, isUpper, isLetter)

--
-- ASSUME: English Alphabet
--
sameCase :: Char -> Char -> Int
sameCase c1 c2
  | any (not . isLetter) [c1, c2] = -1
  | all isLower [c1, c2] = 1
  | all isUpper [c1, c2] = 1
  | otherwise = 0

run :: IO ()
run = do
  putStrLn (show $ sameCase 'z' '@')
  putStrLn (show $ sameCase 'p' 'Q')
  putStrLn (show $ sameCase 'z' 'k')
  putStrLn (show $ sameCase 'K' 'Z')


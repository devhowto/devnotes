{-# LANGUAGE NoMonomorphismRestriction #-}

module CheckSameCase (sameCase, run) where

import Prelude
import Data.Char (isLower, isUpper, isLetter)

--
-- ASSUME: English Alphabet
--
sameCase :: Char -> Char -> Int
sameCase c1 c2
  | not $ isLetter c1 = -1
  | not $ isLetter c2 = -1
  | isLower c1 && isLower c2 = 1
  | isUpper c1 && isUpper c2 = 1
  | otherwise = 0

run :: IO ()
run = do
  putStrLn (show $ sameCase 'z' '@')
  putStrLn (show $ sameCase 'q' 'R')
  putStrLn (show $ sameCase 'z' 'k')
  putStrLn (show $ sameCase 'K' 'Z')

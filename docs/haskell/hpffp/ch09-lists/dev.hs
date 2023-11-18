{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Char (isUpper, toUpper)

str :: [Char]
str = "HbEfLrLxO"

onlyUppers :: [Char] -> [Char]
onlyUppers str = go str []
  where
    go :: [Char] -> [Char] -> [Char]
    go [] loc       = loc
    go (c : cs) loc =
      if isUpper c
      then go cs (c : loc)
      else go cs loc

uppers :: String -> String
uppers = filter isUpper

--
-- λ> onlyUppers "HbEfLrLxO"
-- "HELLO"
--

--
-- ASSUME: The input has length >= 1.
--
capitalize :: [Char] -> [Char]
capitalize []       = ""
capitalize (c : cs) = toUpper c : cs
--
-- λ> capitalize "yoda"
-- "Yoda"
--
-- λ> capitalize "ahsoka tano"
-- "Ahsoka tano"
--

{-
capitalizeAll :: [Char] -> [Char]
capitalizeAll []       =      ""
capitalizeAll (c : cs) =  toUpper c : capitalizeAll cs

capitalizeAll :: [Char] -> [Char]
capitalizeAll = foldr ((:) . toUpper) ""
-}

capitalizeFirst :: [Char] -> Char
capitalizeFirst str = toUpper $ head str
-- capitalizeFirst = toUpper . head

--
-- λ> capitalizeFirst "haskell"
-- 'H'
--

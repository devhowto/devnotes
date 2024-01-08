{-# LANGUAGE NoMonomorphismRestriction #-}

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

--
-- T.C: O(n).
-- S.C: O(1).
--
vowelCount :: [Char] -> Int
vowelCount = length . filter isVowel

main :: IO ()
main = do
  -- No chars, 0 vowels.
  print $ vowelCount ""

  -- No vowels in this string.
  print $ vowelCount "xyz"

  -- All 5 chars are vowels.
  print $ vowelCount "aeiou"

  -- “y” is not considered a vowel in this challenge.
  print $ vowelCount "yaeiou"

  -- No vowels, only consontant chars
  print $ vowelCount "bcdfghjklmnpqrstvwxyz"


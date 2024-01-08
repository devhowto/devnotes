{-# LANGUAGE NoMonomorphismRestriction #-}

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

incIf :: (a -> Bool) -> a -> Int -> Int
incIf f v n = case (f v) of
  True -> (+) n 1
  _    -> n

vowelCount :: [Char] -> Int
vowelCount str = go str 0
  where
    go :: [Char] -> Int -> Int
    go [] cnt           = cnt
    go (chr : []) cnt   = go [] (incIf isVowel chr cnt)
    go (chr : chrs) cnt = go chrs (incIf isVowel chr cnt)

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


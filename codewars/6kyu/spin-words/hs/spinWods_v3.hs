--
-- tags: codewars haskell algorithm string
--

spinWords :: String -> String
spinWords = unwords . map revGE5 . words
  where
    revGE5 :: String -> String
    revGE5 w
      | length w >= 5 = reverse w
      | otherwise     = w


main :: IO ()
main = do
  print $ spinWords "hello you awesomE"

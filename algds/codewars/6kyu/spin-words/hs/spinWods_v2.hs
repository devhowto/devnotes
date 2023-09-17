--
-- tags: codewars haskell algorithm string
--

spinWords :: String -> String
spinWords s = unwords $ map revGE5 $ words s
  where
    revGE5 :: String -> String
    revGE5 w
      | length w >= 5 = reverse w
      | otherwise     = w


main :: IO ()
main = do
  print $ spinWords "hello you"

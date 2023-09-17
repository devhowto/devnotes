--
-- tags: codewars haskell algorithm string
--

revGE5 :: String -> String
revGE5 s
  | length s >= 5 = reverse s
  | otherwise     = s

spinWords :: String -> String
spinWords s = unwords $ map revGE5 $ words s

main :: IO ()
main = do
  print $ spinWords "hello you"

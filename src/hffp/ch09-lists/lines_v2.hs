{-# LANGUAGE MonomorphismRestriction #-}

module Dev where

sentence :: String
sentence = "foo\nbar\nqux\njedi"

myLines :: String -> [] String
myLines str = go str []
  where
    go :: String -> [String] -> [String]
    go s acc
      | null s = acc
      | (==) (head s) '\n' = go (dropWhile (== '\n') s) acc
      | otherwise = go
                    (dropWhile (/= '\n') s)
                    (acc ++ [(takeWhile (/= '\n') s)])

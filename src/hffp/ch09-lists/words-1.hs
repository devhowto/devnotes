{-# LANGUAGE NoMonomorphismRestriction #-}

mySplitBy :: Char -> String -> [String]
mySplitBy delim str = go str []
  where
    go :: String -> [String] -> [String]
    go s acc
          | s == "" = acc
          | head s == delim = go (tail s) acc
          | otherwise = go
                        (dropWhile (/= delim) s)
                        (acc ++ [takeWhile (/= delim) s])
--
-- [λ> myWords "Tomb Raider - The Angel Of Darkness"
-- ["Tomb","Raider","-","The","Angel","Of","Darkness"]
--

--
-- Partially apply mySplitby with a space delimiter.
--
myWords :: String -> [String]
myWords = mySplitBy ' '

--
-- Partially apply mySplitby with a newline delimiter.
--
myLines :: String -> [String]
myLines = mySplitBy '\n'

{-# LANGUAGE NoMonomorphismRestriction #-}

line1 = "Tyger Tyger, burning bright\n"
line2 = "In the forests of the night\n"
line3 = "What immortal hand or eye\n"
line4 = "Could frame thy fearful\
\ symmetry?"

sentences = line1 ++ line2 ++ line3 ++ line4

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

dropUntilNL :: [Char] -> [Char]
dropUntilNL str
  | str == "" = str
  | head str == '\n' = tail str
  | otherwise = dropUntilNL (dropWhile (/= '\n') str)

myLines :: String -> [String]
myLines str = go str []
  where go s acc
          | s == "" = acc
          | otherwise = go (dropUntilNL s)
                           (acc ++ [takeWhile (/= '\n') s])

main :: IO ()
main = do
  print $
    "Are they equal? "
    ++ show (myLines sentences == shouldEqual)


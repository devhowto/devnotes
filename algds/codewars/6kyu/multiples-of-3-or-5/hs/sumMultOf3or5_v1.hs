{-# LANGUAGE NoMonomorphismRestriction #-}

multsOf3or5 :: [Int] -> [Int]
multsOf3or5 = filter (\x -> rem x 3 == 0 || rem x 5 == 0)

mults :: Int -> Int
mults n = sum [xs | xs <- multsOf3or5 [1 .. n - 1]]


main :: IO ()
main = do
  print "Hello, world!"
  print $ show $ mults 10

{-# LANGUAGE NoMonomorphismRestriction #-}

module Invert (inv, run) where

inv :: [Integer] -> [Integer]
inv xs = map f xs
  where f :: Integer -> Integer
        f x = case x >= 0 of
          True -> x
          _ -> (- (x + 10))
--
--f n is an alias for negate.
--

run :: IO ()
run = do
  putStrLn $ show $ inv [1, 2, 3]
  putStrLn $ show $ inv [-1, -2, -3]
  putStrLn $ show $ inv [1, -2, 3, -4]


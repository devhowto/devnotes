{-# LANGUAGE NoMonomorphismRestriction #-}

module Invert (inv, run) where

inv :: [Integer] -> [Integer]
inv = map negate

run :: IO ()
run = do
  putStrLn $ show $ inv [1, 2, 3]
  putStrLn $ show $ inv [-1, -2, -3]
  putStrLn $ show $ inv [1, -2, 3, -4]


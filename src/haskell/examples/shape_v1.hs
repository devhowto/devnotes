module Shape (area) where

data Shape
  = Circle Float Float Float
  | Rectangle Float Float Float

area :: Shape -> Float
area (Circle a b c)    = 1
area (Rectangle a b c) = 2


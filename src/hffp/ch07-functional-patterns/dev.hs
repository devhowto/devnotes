{-# LANGUAGE NoMonomorphismRestriction #-}

import           GHC.Int

-- A nullary data constructor.
data T = D

f :: T -> Bool
f D = False

g :: T -> Bool
g D = True

data Idty a = Id a deriving (Eq, Show)

fnId :: Idty a -> a
fnId (Id x) = x

ignoreId :: Idty a -> Char
ignoreId _ = 'z'

data Prod a b = Prod a b deriving (Show, Eq)

pa :: Prod a b -> a
pa (Prod x _) = x

pb :: Prod a b -> b
pb (Prod _ y) = y

pab :: Prod a b -> (a, b)
pab (Prod x y) = (x, y)

-- fuk :: Prod a b -> (a, a)
-- fuk (Prod x x) = (x, x)

data MySum a b c =
    Fst a
  | Snd b
  | Trd c
  deriving (Eq, Show)

mySumToInt :: MySum a b c -> Int8
mySumToInt (Fst _) = -1
mySumToInt (Snd _) = 0
mySumToInt (Trd _) = 1

toInt :: MySum a b c -> Int8
toInt (Snd _) = 0
toInt _       = -1

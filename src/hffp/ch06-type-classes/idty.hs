{-# LANGUAGE NoMonomorphismRestriction #-}

data Idty a =
  Idty a
  deriving Show

instance Eq a => Eq (Idty a) where
  (==) (Idty v) (Idty v') = v == v'



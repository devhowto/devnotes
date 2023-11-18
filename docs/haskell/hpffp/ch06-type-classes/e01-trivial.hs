{-# LANGUAGE NoMonomorphismRestriction #-}

-- A datatype (not a type class).
data Trivial = Foo | Bar deriving Show

-- Now we endow Trivial data type with an instance
-- of Eq.
instance Eq Trivial where
  Foo  == Foo  = True
  (==) Bar Bar = True
  _    == _    = False

t :: Trivial
t = Foo

k :: Trivial
k = Bar


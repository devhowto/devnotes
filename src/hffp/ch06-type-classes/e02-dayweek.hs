{-# LANGUAGE NoMonomorphismRestriction #-}

data FooBar = Foo | Bar
  deriving Show

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show

data Date = Date DayOfWeek Int
  deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _   _   = False

--
-- We need the unconditional case so we don't end up with a partial
-- function. Partial functions do not generate compile time errors. Only
-- runtime errors. Oh noes!
--
-- Remember to `:set -Wall`.
--

{-
instance Eq Date where
  (==) (Date dayOfWeek dayOfMonth)
       (Date dayOfWeek' dayOfMonth') =
    (==) dayOfWeek dayOfWeek' && (==) dayOfMonth dayOfMonth'
-}

instance Eq Date where
  (==) (Date dow dom) (Date dow' dom') =
    dow == dow' && dom == dom'

d :: Date
d = Date Mon 24
-- λ> d == (Date Mon 24)
-- True
-- λ> d == (Date Mon 25)
-- False
-- λ> d == (Date Fri 24)
-- False


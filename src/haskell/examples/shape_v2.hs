--
-- https://discord.com/channels/280033776820813825/505367988166197268/1097791153794977863
--

data Rectangle = MkRectangle
  { getRekt1 :: Float
  , getRekt2 :: Float
  , getRekt3 :: Float
  , getRekt4 :: Float
  } deriving ( Show )

-- a type         a data constructor
-- VVVV           VVVVVVV
--  VVVVV       VVVVVVVV
--   VVVVVV   VVVVVVVV
data Circle = MkCircle
  { getCenter   :: Float
  , getPosition :: Float
  , getRadius   :: Float
  } deriving ( Show )

data Shape
  = MkShapeCircle Circle
  | MkShapeRectangle Rectangle deriving ( Show )
--  ^^^^^^^^^^^^^^   ^^^^^^^^^^
--  ^^^^^^^^^^^^      ^^^^^^^^^^
--  ^^^^^^^^^          ^^^^^^^^^^
--   another             the type
--  constructor         'Rectangle'
--     like             from above
-- 'MkRectangle'


-- equivalent to what Rotaerk wrote with
-- these new types meaning you'd now need to
-- pattern match over TWO LAYERS of constructors
-- to get at the fields that we've named 'a', 'b', etc
area :: Shape -> Float
area (MkShapeCircle (MkCircle a b c))         = 123
area (MkShapeRectangle (MkRectangle a b c d)) = 456

-- alternatively, given how i've written the 'data' types
-- 'Circle' and 'Rectangle' using record syntax, we can use
-- their field names as functions:
notArea :: Shape -> Float
notArea (MkShapeCircle circle)  = pi * (getRadius circle) ** 3
notArea (MkShapeRectangle rect) = 666.42069

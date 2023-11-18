--
-- Page 181.
-- Exercises: Eq instances
--

{-# LANGUAGE NoMonomorphismRestriction #-}

------------------------------------------------------------------------
-- 1
--
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn n) (TisAn m) = n == m

------------------------------------------------------------------------
-- 2
--
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two n m) (Two x y) = n == x && m == y

------------------------------------------------------------------------
-- 3
--
data StrOrInt =
    TisAnInt Int
  | TisAStr String

instance Eq StrOrInt where
  (==) (TisAnInt j) (TisAnInt k) = j == k
  (==) (TisAStr s1) (TisAStr s2) = s1 == s2
  (==) _ _                       = False

------------------------------------------------------------------------
-- 4
--
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair n m) (Pair j k) = n == j && m == k

------------------------------------------------------------------------
-- 5
--
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple w z) = x == w && y == z

------------------------------------------------------------------------
-- 6
--
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _                     = False

------------------------------------------------------------------------
-- 7
--
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y)     = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _                     = False


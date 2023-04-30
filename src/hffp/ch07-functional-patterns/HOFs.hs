{-# LANGUAGE NoMonomorphismRestriction #-}
--
-- The parens indicate myflip takes a function of “two params a and b
-- and returns a c.”
--
myflip :: (a -> b -> c) -> b -> a -> c
myflip f x y = f y x

--
-- Because a -> b -> c allows (but DOES NOT REQUIRE) different argument
-- types, we can use -, which is a -> a -> a for our a -> b -> c.
--
revSub :: Num a => a -> a -> a
revSub = myflip (-)
-- λ> (flip (-)) 10 1
-- -9
-- λ> revSub 10 1
-- -9

--
-- By fliping the params -, we end up dividing 2 by 10 instead of 10 by
-- 2 in this following example.
revDiv :: Fractional a => a -> a -> a
revDiv = flip' (/)
-- λ> (flip (/)) 10 2
-- 0.2
-- λ> revDiv 10 2
-- 0.2

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

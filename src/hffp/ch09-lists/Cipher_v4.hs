module Cipher4 where

import Data.Char (chr, ord)

--
-- The int value of 'a' in ASCII and UTF-8 is 97.
--
a :: Int
a = 97

--
-- Because the English alphabet contains 26 letters, this is the
-- number we need to “wrap around” when shifting char positions.
--
wrp :: Int
wrp = 26

--
-- Translates the zero-based position of a character `c` in the
-- lowercase English alphabet into its corresponding 0 to 25 numeric
-- mapping.
--
-- Examples:
-- • ‘a’ → 0
-- • ‘z’ → 25
--
toPos :: Char -> Int
toPos c = ord c - a

--
-- Translates the zero-based position `i` into its corresponding
-- lowercase letter in the English Alphabet.
--
-- Examples:
-- •  0 → ‘a’
-- • 25 → ‘z’
--
toChr :: Int -> Char
toChr i = chr $ i + a

--
-- Shifts `c` by `n` positions left or right according to `n` being
-- positive or negative!
--
-- Examples:
--
-- • move 1 'a' → 'b'
-- • move (-1) 'a' → 'z'
-- • move 3 'z' → 'c'
-- • move (-3) 'a' → 'x'
--
move :: Int -> Char -> Char
move n c = toChr $ mod (toPos c + n) wrp

--
-- Applies the Caesar cipher to shift chars `n` positions to the right,
-- essentially encrypting messages.
--
-- ASSUME: Lowercase-only English alphabet letters.
--
caesar :: Int -> [Char] -> [Char]
caesar n = map (move $ abs n)

--
-- Applies the Caesar cipher to shift chars `n` positions to the left,
-- essentially decrypting messages.
--
unCaesar :: Int -> [Char] -> [Char]
unCaesar n = map (move (negate . abs $ n))
--
-- λ> caesar 3 "abc"
-- "def"
--
-- λ> caesar 3 "xyz"
-- "abc"
--
-- λ> caesar (-3) "abc"
-- "xyz"
--
-- λ> caesar (-3) "xyz"
-- "uvw"
--


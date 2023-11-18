module Cipher1 where

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
-- Right-shifts c by n positions.
--
-- Examples:
--
-- • move 1 'a' → 'b'
-- • move 3 'a' → 'd'
-- • move 1 'z' → 'a'
-- • move 3 'z' → 'c'
--
move :: Int -> Char -> Char
move n c = toChr $ mod (toPos c + n) wrp

--
-- Applies the Caesar to `chrs` by shifting each letter `n` positions
-- to the right.
--
-- ASSUME: Lowercase-only English alphabet letters.
--
caesar :: Int -> [Char] -> [Char]
caesar n = map (move n)
--
-- λ> caesar 3 "xyz"
-- "abc"
--
-- λ> caesar 1 "abc"
-- "bcd"
--
-- λ> caesar 3 "abc"
-- "def"
--
-- λ> caesar 1 "xyz"
-- "yza"
--
-- λ> caesar 3 "xyz"
-- "abc"
--

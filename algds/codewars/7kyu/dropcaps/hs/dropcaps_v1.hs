{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Char (toLower, toUpper, isSpace)
import Data.List (dropWhileEnd)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

trimAll :: String -> String
trimAll = unwords . words

upcaseFirst :: [Char] -> [Char]
upcaseFirst "" = ""
upcaseFirst (c : cs) = toUpper c : map toLower cs

dropCap :: [Char] -> [Char]
dropCap str = unwords $ go [] (words str)
  where
    go :: [String] -> [String] -> [String]
    go acc [] = acc
    go acc (s : ss)
      | length s >= 3 = go (acc ++ [upcaseFirst s]) ss
      | otherwise     = go (acc ++ [s]) ss

main :: IO ()
main = do
  putStrLn $ dropCap "  hello   world"
  putStrLn $ dropCap "a banana   "
  putStrLn $ dropCap "  a b   c may the FORCE     be with YOU      "

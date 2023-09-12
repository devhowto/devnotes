{-# LANGUAGE NoMonomorphismRestriction #-}

incIf :: Char -> Char -> Int -> Int
incIf c1 c2 n
  | c1 == c2 = (+ 1) n
  | otherwise = n

--
-- In go, instead of checking if the length is 0 and then using head and
-- tail later, consider pattern matching instead (which also allows GHC
-- to warn you about missing patterns).
--
validParens :: String -> Bool
validParens str = go str 0 0
  where
    go :: String -> Int -> Int -> Bool
    go [] l r = l == r
    go (h : t) l r
      | r > l = False
      | otherwise = go
                    t
                    (incIf h '(' l)
                    (incIf h ')' r)

main :: IO ()
main = do
  print $ validParens ""                -- => True
  print $ validParens "("               -- => False
  print $ validParens "(("              -- => False
  print $ validParens ")(()))"          -- => False
  print $ validParens "()"              -- => True
  print $ validParens "()()"            -- => True
  print $ validParens "((()))"          -- => True
  print $ validParens "())()("          -- => False
  print $ validParens "(())((()())())"  -- => True



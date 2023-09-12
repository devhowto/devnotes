{-# LANGUAGE NoMonomorphismRestriction #-}

incIf :: Char -> Char -> Int -> Int
incIf c1 c2 n
  | c1 == c2 = (+ 1) n
  | otherwise = n

validParens :: String -> Bool
validParens str = go str 0 0
  where
    go :: String -> Int -> Int -> Bool
    go s l r
      | length s == 0 = l == r
      | r > l = False
      | otherwise = go
                    (tail s)
                    (incIf (head s) '(' l)
                    (incIf (head s) ')' r)

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

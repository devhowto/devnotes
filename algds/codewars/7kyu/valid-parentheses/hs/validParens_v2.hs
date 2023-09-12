{-# LANGUAGE NoMonomorphismRestriction #-}

validParens :: String -> Bool
validParens = (== 0) . foldr
              (\c n ->
                  if n < 0
                  then n
                  else
                    if c == ')'
                    then n + 1
                    else n - 1) 0

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


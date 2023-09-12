{-# LANGUAGE NoMonomorphismRestriction #-}

--
-- Partially apply flipped cons to an empty list. All we've got to do
-- now is to provide the actual list to reverse.
--
-- λ> :t (:)
-- (:) :: a -> [a] -> [a]
--
-- λ> :t (flip (:))
-- (flip (:)) :: [a] -> a -> [a]
--
rev :: [Char] -> [Char]
rev = foldl (flip (:)) []

run :: IO ()
run = do
  putStrLn $ show $ rev "hello"
  putStrLn $ show $ rev "world"
  putStrLn $ show $ rev "ana"

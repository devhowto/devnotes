{-# LANGUAGE NoMonomorphismRestriction #-}

f :: Bool -> Maybe Int
f False = Just 0
f _     = Nothing

g :: Maybe Int -> String
g (Just n) = "Value: " ++ (show n)
g Nothing  = "Hmm..."
-- λ> g Nothing
-- "Hmm..."
-- λ> g (Just 1)
-- "Value: 1"
-- λ> g $ Just 1
-- "Value: 1"

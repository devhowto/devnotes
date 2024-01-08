{-# LANGUAGE NoMonomorphismRestriction #-}

removeChar :: String -> String
removeChar = (take . subtract 2 . length) <*> (drop 1)

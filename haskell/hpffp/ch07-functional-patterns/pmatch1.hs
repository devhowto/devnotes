{-# LANGUAGE NoMonomorphismRestriction #-}

data TJedi =
    Padawan
  | Master
  deriving (Eq, Show)

isPadawan :: TJedi -> Bool
isPadawan Padawan = True
isPadawan _       = False

isMaster :: TJedi -> Bool
isMaster Master = True
isMaster _      = False

data Jedi = Jedi TJedi deriving (Eq, Show)

getType :: Jedi -> TJedi
getType (Jedi t) = t

-- λ> getType (Jedi Master)
-- Master
-- λ> getType (Jedi Padawan)
-- Padawan

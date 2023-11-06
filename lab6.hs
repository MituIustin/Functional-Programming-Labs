data Fruct = Mar String Bool
            | Portocala String Int

cosFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden Delicious" True, Portocala "Sanguinello" 15, Portocala "Moro" 12, Portocala "Tarocco" 3, Portocala "Moro" 12, Portocala "Valencia" 2, Mar "Golden Delicious" False, Mar "Golden" False, Mar "Golden" True]

{- 1a -}

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala x1 x2) = x1 == "Tarrocco" || x1 == "Moro" || x1 == "Sanguinello"
ePortocalaDeSicilia _ = False

{- 1b -}

nr_felie :: Fruct -> Int
nr_felie (Portocala _ x2) = x2

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs) = if ePortocalaDeSicilia x 
                            then nr_felie x + nrFeliiSicilia xs
                            else nrFeliiSicilia xs

{- 1c -}

areViermi :: Fruct -> Bool
areViermi (Mar _ x1) = x1

nrMereViermi :: [Fruct] -> Int
nrMereViermi xs = length (filter areViermi xs)

{- 2 -}


type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

{- 2a -}

vorbeste :: Animal -> String
vorbeste (Caine _ _ ) = "Woof!"
vorbeste (Pisica _) = "Meow!"

{- 2b -}

rasa :: Animal -> Maybe String
rasa (Caine _ x ) = Just(x)
rasa (Pisica _) = Nothing


{- 3 -}

data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

{- 3a -}

verifica :: Matrice -> Int -> Bool
verifica (M matrice) x = foldr verificaLinie True matrice
  where
    verificaLinie (L linie) acc = (sum linie == x) && acc

{- 3b -}

doarPozN :: Matrice -> Int -> Bool
doarPozN (M matrice) x = foldr verificaLinie True matrice
    where 
        verificaLinie (L linie) acc = if length linie == x 
                                        then  
                                            if length (filter (>0) linie) == x
                                                    then True && acc
                                            else False && acc
                                        else 
                                            True && acc
                                        

{- 3c -}
 
corect :: Matrice -> Bool
corect (M matrice) = verificaLungime matrice
  where
    verificaLungime [] = True
    verificaLungime [x] = True  
    verificaLungime (x:y:xs) = (lengthLinie x == lengthLinie y) && verificaLungime (y:xs)
    
    lengthLinie (L linie) = length linie


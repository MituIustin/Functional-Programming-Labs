import Data.Char (digitToInt, isDigit)
import Data.List ()  

{- ex1 -}

nrVocaleCuvant :: [Char] -> Int
nrVocaleCuvant [] = 0
nrVocaleCuvant (x:xs) = if elem x "aeiou"
                            then 1 + nrVocaleCuvant xs
                            else nrVocaleCuvant xs

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs) = if x == reverse x
                    then nrVocaleCuvant x + nrVocale xs
                    else nrVocale xs

{- ex2 -}

f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x:xs) = if even x
            then x:n:(f n xs)
            else x:(f n xs)

{- ex3 -}

divizori :: Int -> [Int]
divizori x = [div | div <- [1..x] , mod x div == 0]

{- ex4 -}

listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (x:xs) = divizori x : listadiv xs

{- ex5a -}

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b xs = if head xs >= a && head xs <= b
                            then head xs : inIntervalRec a b (tail xs )
                            else inIntervalRec a b ( tail xs)

{- ex5b -}

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b xs = [nr | nr <- xs, nr >= a, nr <= b]

{- ex6a -}

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec xs = if head xs > 0
                    then 1 + pozitiveRec (tail xs)
                    else pozitiveRec (tail xs)

{- ex6b -}

pozitiveComp :: [Int] -> Int
pozitiveComp xs = length [x | x <- xs, x > 0]

{- ex7a -}

pozitie :: Int -> [Int] -> [Int]
pozitie _ [] = []
pozitie n xs = if odd (head xs)
                then n : pozitie (n + 1) (tail xs)
                else pozitie (n + 1) (tail xs)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec xs = pozitie 0 xs

{- ex7b -}

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp xs = [b | (a,b) <- zip xs [0..] , odd a]
                    

{- ex8a -}

multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1
multDigitsRec xs = if head xs `elem` "0123456789"
                        then digitToInt (head xs) * multDigitsRec (tail xs)
                        else multDigitsRec (tail xs)

{- ex8b -}

multDigitsComp :: [Char] -> Int
multDigitsComp xs = product [digitToInt x | x <- xs, isDigit x]

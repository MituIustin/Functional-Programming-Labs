
{- ex2 -}
factori :: Int -> [Int]
factori x = [y | y <- [1..x], mod x y == 0]

{- ex3 -}
prim :: Int -> Bool
prim  x = length (factori x ) == 2 && x >= 2

{- ex4 -}
numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

{- ex5 -}
minim ::  [Int] -> [Int] -> [Int] -> Int
minim xs ys zs = min (length xs) (min (length ys) (length zs))

newfunct ::  [Int] -> [Int] -> [Int] -> Int -> [(Int, Int, Int)]
newfunct (x:xs) (y:ys) (z:zs) 1 = [(x,y,z)]
newfunct (x:xs) (y:ys) (z:zs) n = (x,y,z) : newfunct xs ys zs (n - 1)

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 xs ys zs= newfunct xs ys zs (minim xs ys zs)

{- ex6 -}
firstEl :: [(a,b)] -> [a]
firstEl xs = map (\(a,_) -> a) xs

{- ex7 -}
sumList :: [[Int]] -> [Int]
sumList xs = map sum xs

{- ex8 -}
pre12 :: [Int] -> [Int]
pre12 xs = map (\x -> if even x then div x 2 
                    else x * 2) xs

{- ex9 -}
siruri :: [[Char]] -> Char -> [[Char]]
siruri xs x = filter (elem x) xs

{- ex10 -}
imparep :: [Int] -> [Int]
imparep xs = map (\x -> x * x) (filter odd xs)

{- ex11 -}
ex11 :: [Int] -> [Int]
ex11 xs = map (\(x,_) -> x * x) (filter (\(x,y) -> odd y) (zip xs [1..]))

{- ex12 -}
cuv :: [Char] -> [Char]
cuv xs = filter (\x -> elem x "aeiouAEIOU") xs

numaiVocale :: [[Char]] -> [[Char]]
numaiVocale xs = map cuv xs

{- ex13 -}
mymap :: (a->b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = (f x ) : mymap f xs 

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) = if f x then x : myfilter f xs 
                    else myfilter f xs

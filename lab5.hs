{- ex1 -}

sum_squares :: [Int] -> Int
sum_squares xs = foldr (+) 0
                    (map (^2) 
                     (filter (>0) xs))

{- ex2 -}

all_true :: [Bool] -> Bool
all_true xs = foldr (&&) True xs

{- ex3 -}

prop :: Int -> Bool
prop x = even x

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies prop xs = if length xs == length (filter prop xs)
                        then True
                        else False


{- ex4 -}

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies prop xs = if length (filter prop xs) > 0 
                        then True
                        else False

{- ex5 -}

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr functie xs = foldr (\x acc -> functie x : acc) [] xs

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr functie xs = foldr (\x acc -> if functie x then x : acc else acc) [] xs

{- ex6 -}

functie :: Integer -> Integer -> Integer
functie a b = a*10 + b

listToInt :: [Integer] -> Integer
listToInt xs = foldl (functie) 0 xs

{- ex7a -}

rmChar :: Char -> String -> String
rmChar ch str = foldr (\x acc -> if ch /= x then x : acc else acc) [] str

{- ex7b -}

rmCharsRec :: String -> String -> String
rmCharsRec [] list = list
rmCharsRec (x:xs) list = rmChar x (rmCharsRec xs list)

{- ex7c -}

rmCharsFold :: String -> String -> String
rmCharsFold removeChars str = foldr rmChar str removeChars

{- ex8 -}

myReverse :: [Int] -> [Int]
myReverse xs = foldl (\ acc x -> x : acc) [] xs

{- ex9 -}

myElem :: Int -> [Int] -> Bool
myElem x xs = if length (filter (/=x) xs) > 0 
                then True
                else False

{- ex10 -}

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip lista = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys)) ([], []) lista

{- ex11 -}

union :: [Int] -> [Int] -> [Int]
union l1 l2 = (foldr(\x xs -> if elem x l2 then xs else x:xs) [] l1 ) ++ l2
                
{- ex12 -}

intersect :: [Int] -> [Int] -> [Int]
intersect l1 l2 = (foldr(\x xs -> if elem x l2 then x:xs else xs) [] l1 ) 

{- ex13 -}

permutations :: [a] -> [[a]]
permutations xs = foldr (\x acc -> concatMap (insertEverywhere x) acc) [[]] xs
  where
    insertEverywhere :: a -> [a] -> [[a]]
    insertEverywhere x [] = [[x]]
    insertEverywhere x (y:ys) = (x:y:ys) : map (y:) (insertEverywhere x ys)

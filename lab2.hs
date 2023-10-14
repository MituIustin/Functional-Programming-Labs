{- ex1 -}
poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a*x*x + b*x + c


{- ex 2 -}
eeny :: Integer -> String
eeny x = if(even x) 
    then "eeny"
    else "meeny"


{- ex3 -}
fizzbuzz :: Integer -> String
fizzbuzz x 
    | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
    | mod x 3 == 0 = "Fizz"
    | mod x 5 == 0 = "Buzz"
    | otherwise = ""

fizzbuzz :: Integer -> String
fizzbuzz x = if(mod x 3 == 0 && mod x 5 == 0)
    then "FizzBuzz"
    else 
        if(mod x 3 == 0)
            then "Fizz"
            else 
                if(mod x 5 == 0)
                    then "Buzz"
                    else ""


{- ex4 -}
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci x = tribonacci(x-1) +  tribonacci(x-2) + tribonacci(x-3)


{- ex5 -}
binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k = binomial(n - 1) (k) + binomial (n - 1) (k - 1)


{- ex6a -}
verifL :: [Int] -> Bool
verifL xs = if(mod (length xs) 2 == 0) 
    then True
    else False


{- ex6b -}
takefinal :: [Int] -> Int -> [Int]
takefinal (x:xs) n = if(length xs < n)
    then x:xs
    else
        if(length xs == n)
            then xs
            else takefinal xs n

{- ex6c -}
remove :: [Int] -> Int -> [Int]
remove xs n = take (n-1) xs ++ drop n xs


{- ex7a -}
myreplicate :: Int -> Int -> [Int]
myreplicate n v = if(n == 0)
    then []
    else v:(myreplicate (n-1) v)


{- ex7b -}
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (x:xs) = if(even x)
    then sumImp xs
    else x + sumImp xs

{- ex7c -}
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (x:xs) = if(take 1 x == "A")
    then length x + totalLen xs
    else totalLen xs

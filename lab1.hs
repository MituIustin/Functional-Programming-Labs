{- 8a -}

patrate a b = a * a + b * b

{- 8b -}

paritate x = if even x
            then "par"
            else "impar"

{- 8c -}

fact 1 = 1
fact x = x * fact(x-1)

{- 8d -}

verif a b = a > 2 * b

{- 8e -}

maxim [] = -1
maxim (x:xs) = max x (maxim xs)

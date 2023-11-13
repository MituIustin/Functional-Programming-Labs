data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
    deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
    | Node Operation Tree Tree -- branch
    deriving (Eq, Show)

instance Show Expr where
    show (Const x) = show x
    show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"


{- ex1 -}

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2


{- ex2 -}

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add a b) = (evalArb a) + (evalArb b)
evalArb (Node Mult a b) = (evalArb a) * (evalArb b)


{- ex3 -}

expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (e1 :+: e2) = Node Add  (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)



data IntSearchTree value = Empty
    | BNode
    (IntSearchTree value)   -- elemente cu cheia mai mica
    Int                     -- cheia elementului
    (Maybe value)           -- valoarea elementului
    (IntSearchTree value)   -- elemente cu cheia mai mare


{- ex4 -}

lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' _ Empty = Nothing 
lookup' key (BNode leftTree currentKey currentValue rightTree)
    | key == currentKey = currentValue  
    | key < currentKey = lookup' key leftTree  
    | otherwise = lookup' key rightTree

{- ex5 -}

keys :: IntSearchTree value -> [Int]
keys Empty = []
keys (BNode leftTree currentKey _ rightTree) = keys leftTree ++ [currentKey] ++ keys rightTree


{- ex6 -}

values :: IntSearchTree value -> [value]
values Empty = []
values (BNode leftTree _ (Just value) rightTree) = values leftTree ++ [value] ++ values rightTree
values (BNode leftTree _ Nothing rightTree) = values leftTree ++ values rightTree

{- ex7 -}

insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert x y Empty = BNode Empty x (Just y) Empty
insert x y (BNode leftTree key value rightTree)
    | x == key = BNode leftTree x (Just y) rightTree  
    | x < key = BNode (insert x y leftTree) key value rightTree  
    | otherwise = BNode leftTree key value (insert x y rightTree) 

{- ex8 -}


{- ex9 -}

toList :: IntSearchTree value -> [(Int, value)]
toList Empty = [] 
toList (BNode leftTree key (Just value) rightTree) =
    toList leftTree ++ [(key, value)] ++ toList rightTree
toList (BNode leftTree key Nothing rightTree) =
    toList leftTree ++ toList rightTree

{- ex10 -}

fromList :: [(Int, value)] -> IntSearchTree value
fromList [] = Empty  
fromList ((key, value):xs) =
    let leftPairs = filter (\(k, _) -> k < key) xs  
        rightPairs = filter (\(k, _) -> k > key) xs  
        leftTree = fromList leftPairs  
        rightTree = fromList rightPairs  
    in BNode leftTree key (Just value) rightTree  

{- ex11 -}


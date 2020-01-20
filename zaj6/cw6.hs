-- Napisać definicję funkcji obliczającej n-ty wyraz ciągu danego wzorem an=an-1+2an-2 (a1=0,
-- a2=5) a) rekurencyjną, b) w wersji „akumulatorowej”.

zad1a x
    | x == 1 = 0
    | x == 2 = 5
    | otherwise = zad1a (x-1) + 2*(zad1a(x-2))

zad1b x = zad1bHelper x 0
zad1bHelper x acc 
    | x == 1 = acc
    | otherwise = zad1bHelper (x - 1) (zad1a (x-1) + 2*(zad1a(x-2)) + acc)

silnia n = liczSil n 1

liczSil n acc
    | n == 0 = acc
    | otherwise = liczSil (n-1) (n * acc)

-- Napisać definicję funkcji, która w liście przestawia a) pierwszy element z drugim, b) pierwszy
-- element z ostatnim, c) drugi element z przedostatnim.

zad2a arr = (head (tail arr)) : [head arr] ++ tail(tail(arr))
zad2b arr = (last arr) : init(tail arr) ++ [head(arr)]
zad2c arr = (head arr) : last(init(arr)) : init(tail(init(tail(arr)))) ++ [head(tail(arr))] ++ [last(arr)]

-- Napisać definicję funkcji, której wartością jest liczba wystąpień elementu d w liście l.
-- Np. f ‘a’ [‘a’,’b’,’a’,’c’,’a’]=3

zad3 n arr = sum (map (\y -> 1) (filter (\x -> x == n) arr))

-- Napisać funkcję sprawdzającą równość dwóch list.
-- Np. f [1,2,3,4] [1,2,3,4]=True
--  f [1,2,3,4] [4,3,2,1]=False

zad4 [] [] = True
zad4 (x:xs) (y:ys)
    | x == y = zad4 xs ys
    | otherwise = False

-- Napisać funkcję sprawdzającą równość dwóch zbiorów.
-- Np. f [1,2,3,4] [4,3,2,1]=True
--  f [1,2,3,4] [5,1,2,3]=False

zad5 [] _ = True
zad5 (x:xs) y
    | x `elem` y = zad5 xs y
    | otherwise = False

-- Napisać funkcję, która dla dwóch uporządkowanych niemalejąco list liczbowych l1 i l2 daje
-- w wyniku uporządkowaną niemalejąco listę elementów z list l1 i l2 .
-- Np. f [1,3,5,8,10] [0,2,6,9]=[0,1,2,3,5,6,8,9,10]

zad6 [] y = y
zad6 x [] = x
zad6 (x:xs) (y:ys)
    | x <= y = x : zad6 xs (y:ys)
    | otherwise = y : zad6 (x:xs) ys

-- Napisać funkcję, która dla dwóch drzew binarnych d1 i d2 zwraca wartość True, gdy drzewo
-- d1 jest poddrzewem drzewa d2.   

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)  

numTree1 :: Tree Int 
numTree1 =   
    Node 3  
        (Node 7 Empty Empty) 
        (Node 8 Empty Empty)

numTree2 :: Tree Int 
numTree2 =   
    Node 1  
        (Node 3 
            (Node 7 Empty Empty) 
            (Node 8 Empty Empty)
        )
        (Node 5 
            (Node 4 Empty Empty)
            (Node 6 Empty Empty)
        )

zad7 :: Eq a => Tree a -> Tree a -> Bool
zad7 _ Empty = False
zad7 (Node root1 l1 r1) (Node root2 l2 r2)
    | root1 == root2 && l1 == l2 && r1 == r2 = True
    | otherwise = zad7 (Node root1 l1 r1) l2 || zad7 (Node root1 l1 r1) r2


-- Dla danej listy l wypisać listę par [(element, liczba wystąpień), …].
-- Np. f [2,3,4,1,2,5,3,2,4,4,2 ] = [(2,4),(3,2),(4,3),(1,1),(5,1)]
--  f [‘a’,’a’,’b’,’a’] = [(‘a’,3),(‘b’,1)]

withoutDuplicates [] = []
withoutDuplicates (x:xs) = x : withoutDuplicates (filter (/=x) xs)

numTimesFound [] _ = []
numTimesFound (x:xs) y = [sum (map (\a -> 1) ( filter (x==) y))] ++ numTimesFound xs y

zad8 x = zip a (numTimesFound a x)
    where
        a = withoutDuplicates x



import Computations

zad1a = podzbior [1,2,3] [5,2,6,1,8,3] 
zad1b = podzbior [1,2,3] [5,1,6,2,4] 
zad1c = iloczyn [1,2,3,4] [5,3,8,7,1]  
zad1d = suma [1,2,3,4] [5,3,2,7,1] 
zad1e = roznica [1,2,3,4] [5,3,2,7,1] 
zad1f = roznica [5,3,2,7,1] [1,2,3] 

-- Napisać funkcję, która dla drzewa binarnego d zwraca True, gdy drzewo ma w węzłach
-- liczby całkowite i wszystkie elementy w lewym poddrzewie są mniejsze od liczby
-- w korzeniu, a w prawym poddrzewie – większe.

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)  

numTree :: Tree Int 
numTree =   
    Node 4  
        (Node 1 
            (Node 6 Empty Empty) 
                Empty)
        (Node 5 Empty Empty)
        
checkRight :: Ord a => a -> Tree a -> Bool
checkLeft :: Ord a => a -> Tree a -> Bool

checkLeft x Empty = True
checkLeft x (Node a l r)
    | a < x = checkLeft x l && checkLeft x r 
    | otherwise = False

checkRight x Empty = True
checkRight x (Node a l r)
    | a > x = checkRight x l && checkRight x r 
    | otherwise = False

isValidTree :: Ord a => Tree a -> Bool
isValidTree (Node root l r) = checkLeft root l && checkRight root r
        

-- Dla danej listy l wypisać listę par [(element, liczba wystąpień), …].
-- Przykład: w [2,3,4,1,2,5,3,2,4,4,2 ] = [(2,4),(3,2),(4,3),(1,1),(5,1)]
--  w [‘a’,’a’,’b’,’a’] = [(‘a’,3),(‘b’,1)]

withoutDuplicates [] = []
withoutDuplicates (x:xs) = x : withoutDuplicates (filter (/=x) xs)

numTimesFound [] _ = []
numTimesFound (x:xs) y = [sum (map (\a -> 1) ( filter (x==) y))] ++ numTimesFound xs y

countOccurenes x = zip a (numTimesFound a x)
    where
        a = withoutDuplicates x



zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


unzip' [] = ([],[])
unzip' x = (map fst (x),map snd (x))


-- foldr (/) 2 [6, 12, 24, 8]
-- (/) 6 (foldr (/) 2 [12, 24, 8])
-- (/) 6 ((/) 12 (foldr (/) 2 [24, 8]))
-- (/) 6 ((/) 12 ((/) 24 (foldr (/) 2 [8])))
-- (/) 6 ((/) 12 ((/) 24 ((/) 8 (foldr (/) 2 []))))
-- (/) 6 ((/) 12 ((/) 24 ((/) 8 2)))
-- (/) 6 ((/) 12 ((/) 24 4))
-- (/) 6 ((/) 12 6)
-- (/) 6 2
-- 3

-- foldr (&&) True [1>2, 3>2, 5==5]
-- (&&) (1>2) (foldr (&&) True [3>2, 5==5])
-- (&&) (1>2) ((&&) (3>2) (foldr (&&) True [5==5]))
-- (&&) (1>2) ((&&) (3>2) ((&&) (5==5) (foldr (&&) True [])))
-- (&&) (1>2) ((&&) (3>2) ((&&) (5==5) True))
-- (&&) (1>2) ((&&) (3>2) False)
-- (&&) (1>2) False
-- False

-- foldr max 18 [3, 6, 12, 4, 55, 11]
-- max 3 (foldr max 18 [6, 12, 4, 55, 11])
-- max 3 (max 6 (foldr max 18 [12, 4, 55, 11]))
-- max 3 (max 6 (max 12 (foldr max 18 [4, 55, 11])))
-- max 3 (max 6 (max 12 (max 4 (foldr max 18 [55, 11]))))
-- max 3 (max 6 (max 12 (max 4 (max 55 (foldr max 18 [11])))))
-- max 3 (max 6 (max 12 (max 4 (max 55 (max 11 (foldr max 18 []))))))
-- max 3 (max 6 (max 12 (max 4 (max 55 (max 11 18)))))
-- max 3 (max 6 (max 12 (max 4 (max 55 18))))
-- max 3 (max 6 (max 12 (max 4 55)))
-- max 3 (max 6 (max 12 55))
-- max 3 (max 6 55)
-- max 3 55
-- 55


-- foldr (\x y -> (x+y)/2) 54 [24, 4, 10, 6]
-- (\x y -> (x+y)/2) 24 (foldr (\x y -> (x+y)/2) 54 [4,10,6])
-- (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 (foldr (\x y -> (x+y)/2) 54 [10,6]))
-- (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 ((\x y -> (x+y)/2) 10 (foldr (\x y -> (x+y)/2) 54 [6] )))
-- (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 ((\x y -> (x+y)/2) 10 ((\x y -> (x+y)/2) 6 (foldr (\x y -> (x+y) /2) 54 []))))
-- (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 ((\x y -> (x+y)/2) 10 ((\x y -> (x+y)/2) 6 54)))
-- (\x y -> (x+y)/2) 24 ((\x y -> (x+y)/2) 4 ((\x y -> (x+y)/2) 10 30))
-- (\x y -> (x+y)/2) 24 12
-- 18

-- foldl (\x y -> (x+y)/2) 54 [2, 4, 10, 6]
-- foldl (\x y -> (x+y)/2) ((\x y -> (x+y)/2) 54 2) [4, 10, 6]
-- foldl (\x y -> (x+y)/2) ((\x y -> (x+y)/2) ((\x y -> (x+y)/2) 54 2) 4) [10, 6]
-- foldl (\x y -> (x+y)/2) ((\x y -> (x+y)/2) ((\x y -> (x+y)/2) ((\x y -> (x+y)/2) 54 2) 4) 10) [6]
-- foldl (\x y -> (x+y)/2) ((\x y -> (x+y)/2) ((\x y -> (x+y)/2) ((\x y -> (x+y)/2) ((\x y -> (x+y)/2) 54 2) 4) 10) 6) [] 

-- foldl (/) ((/) 64 4) [2, 4]
-- foldl (/) ((/) ((/) 64 4) 2) [4]
-- foldl (/) ((/)((/)((/) 64 4) 2 )4) []
-- ((/)((/)((/) 64 4) 2 )4)
-- ((/)((/)16 2 )4)
-- ((/)8 4)
-- 2


-- foldl (\x y -> 2*x + y) 8 [1, 2, 3]
-- foldl (\x y -> 2*x + y) ((\x y -> 2*x + y) 8 1) [2,3]
-- foldl (\x y -> 2*x + y) ((\x y -> 2*x + y)((\x y -> 2*x + y) 8 1) 2) [3]
-- foldl (\x y -> 2*x + y) ((\x y -> 2*x + y)((\x y -> 2*x + y)((\x y -> 2*x + y) 8 1) 2) 3) []
-- ((\x y -> 2*x + y)((\x y -> 2*x + y)((\x y -> 2*x + y) 8 1) 2) 3)
-- ((\x y -> 2*x + y)((\x y -> 2*x + y)17 2) 3)
-- ((\x y -> 2*x + y) 36 3)
-- 75
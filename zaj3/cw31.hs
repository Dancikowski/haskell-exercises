-- Zadanie 1
f1 = \a -> \b -> \c -> a + b + c 
f2 arr =  map (\a -> a + 2) arr
f3 arr = filter (\a -> a `mod` 2 == 0) arr


-- Zadanie 2 a
data Moto = Honda | Ford | Audi | Seat | Bentley deriving(Show)
type Kraj = [Char]

getCarName:: Kraj -> Moto

getCarName country = case country of
    "Japan" -> Honda
    "USA" -> Ford
    "Germany" -> Audi
    "Spain" -> Seat
    "UK" -> Bentley

-- Zadanie 2 b
type MaxSpeed = Float

getMaxSpeed:: Moto -> MaxSpeed

getMaxSpeed m = case m of
    Honda -> 200.2
    Ford -> 199.28
    Audi -> 278.0
    Seat -> 111.11
    Bentley -> 299.99

-- Zadanie 3 a

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  

numTree :: Tree Int 
numTree =   
    Node 1  
        (Node 2  
            (Node 4 Empty Empty) 
            (Node 5  Empty  
                (Node 8 Empty Empty)  
            )
        )  
        (Node 3  
            (Node 6 Empty   
                (Node 9 Empty Empty)  
            )  
            (Node 7 Empty Empty)  
        ) 


preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a l r) = [a] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a l r) = inorder l ++ [a] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a l r) = postorder l ++ postorder r ++ [a]

-- Zadanie 3b

charTree:: Tree Char
charTree = 
    Node 'a'
        (Node 'b' Empty
            (Node 'd' 
            	(Node 'f' Empty Empty) 
            	    Empty)
        )
        (Node 'c' 
        	(Node 'e' Empty
        		(Node 'g' Empty Empty)) 
        	Empty)


-- Zadanie 4a
treeMember tree a = a `elem` preorder tree

-- Zadanie 4b


--treeMember' Empty _ = False
--treeMember' (Node a l r) el
--    | a == el = True
--    | otherwise = treeMember' l 
-- https://www.codewars.com/kata/58ad317d1541651a740000c5/train/


-- Zadanie 5 a

height' Empty = 0
height' (Node a l r) = 1 + (max (height' l)(height' r))


-- Zadanie 5 b 

minHeight' Empty = 0
minHeight' (Node a l r) = 1 + (min (minHeight' l)(minHeight' r))

-- Zadanie 6
-- traverseBF :: Tree a -> [a]
bfs tree = traverse [tree]
    where
        traverse [] = []
        traverse xs = map (\ (Node a _ _) -> a) xs ++ traverse (concat (map calcNodes xs))

        calcNodes (Node _ Empty Empty) = []
        calcNodes (Node _ Empty b)     = [b]
        calcNodes (Node _ a Empty)     = [a]
        calcNodes (Node _ a b)         = [a,b]


bfs2 tree = fun [tree]
    where
        fun x = concat (map calcNodes x)

        calcNodes (Node _ Empty Empty) = []
        calcNodes (Node _ Empty b)     = [b]
        calcNodes (Node _ a Empty)     = [a]
        calcNodes (Node _ a b)         = [a,b]

-- data MyType = Damian | Lasecki deriving(Show)
-- type Input = [Char]

-- getDamian::Input -> MyType

-- getDamian var = case var of
--     "imie" -> Damian
--     "nazw" -> Lasecki 



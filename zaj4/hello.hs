import Data.Char 
import Control.Monad
import Data.Maybe

main = do
    putStrLn "Hello what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ " ,you rock!" ) 

a = do 
    x <- getChar
    y <- getChar
    return (x,y)

talk = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" 

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z

compareWithHundread :: (Num a, Ord a) => a -> Ordering
compareWithHundread = compare 100 

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x


echo :: IO ()
echo = do 
    line <- getLine
    putStr line

palindrom :: IO ()
palindrom = do
    putStr "Napisz cos: "
    line <- getLine
    let line' = filter (not . (==) ' ' ) (map toLower line)
    if line' == reverse line'
        then putStr ("'" ++ line ++ "'jest palindromem!\n'")
        else putStr ("Nie jest")


strlen :: IO ()
strlen = do 
    putStr "Napisz cos: "
    xs <- getLine
    putStr "String ma "
    putStr (show (length xs))
    putStrLn " znakow"



sq = do
    rs <- sequence [getLine, getLine]
    print rs

for = forever $ do
    putStr "Elo: "
    k <- getLine
    putStrLn $ map toUpper k


aha = do 
    x <- [1,2,3]
    y <- [1,2,3]
    True <- return (x /= y)
    return (x,y)

-- data Maybe a = Nothing | Just a

-- safediv :: Int -> Int -> Maybe Int
-- safediv _ 0 = Nothing
-- safediv x y = Just (x `div` y)


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs


any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs)
    | p x = True
    | otherwise = any' p xs


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = (x:xs)


getLine' :: IO String
getLine' = do
    x <- getChar
    if x == '\n' then return []
    else 
        do 
            xs <- getLine'
            return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do
    putStr' xs
    putChar '\n'

print' :: Show a => a -> IO ()
print' x = putStrLn'(show x)

-- data Maybe a = Just a | Nothing

-- instance Monad Maybe where
--     return = Just
--     Nothing >>= m = Nothing
--     (Justa a) >>= m = m a

-- foldr (&&) True [1>2, 3>2, 5==5]
-- False
-- ==> 1>2 && (foldr (&&) True [3>2, 5==5])
-- ==> 1>2 && (3>2 && foldr (&&) True [5==5])
-- ==> 1>2 && (3>2 && (5==5 && foldr (&&) True []))
-- ==> 1>2 && (3>2 && (5==5 && True))
-- ==> 1>2 && (3>2 && True)
-- ==> 1>2 && True
-- ==> False
-- foldr max 18 [3, 6, 12, 4, 55, 11]
-- 55
-- ==> 3 max (foldr max 18 [6, 12, 4, 55, 11])
-- ==> 3 max (6 max (foldr max 18 [12, 4, 55, 11]))
-- ==> 3 max (6 max (12 max (foldr max 18 [4, 55, 11])))
-- ==> 3 max (6 max (12 max (4 max (foldr max 18 [55, 11]))))
-- ==> 3 max (6 max (12 max (4 max (55 max (foldr max 18 [11])))))
-- ==> 3 max (6 max (12 max (4 max (55 max (11 max foldr 18 [])))))
-- ==> 3 max (6 max (12 max (4 max (55 max (11 max 18)))))
-- ==> 3 max (6 max (12 max (4 max (55 max 18))))
-- ==> 3 max (6 max (12 max (4 max 55)))
-- ==> 3 max (6 max (12 max 55))
-- ==> 3 max (6 max 55)
-- ==> 3 max 55
-- ==> 55
-- foldr max 81 [3, 6, 12, 4, 55, 11]
-- 81
-- ==> 3 max (foldr max 81 [6, 12, 4, 55, 11])
-- ==> 3 max (6 max (foldr max 81 [12, 4, 55, 11]))
-- ==> 3 max (6 max (12 max (foldr max 81 [4, 55, 11])))
-- ==> 3 max (6 max (12 max (4 max (foldr max 81 [55, 11]))))
-- ==> 3 max (6 max (12 max (4 max (55 max (foldr max 81 [11])))))
-- ==> 3 max (6 max (12 max (4 max (55 max (11 max (foldr max 81 []))))))
-- ==> 3 max (6 max (12 max (4 max (55 max (11 max 81)))))
-- ==> 3 max (6 max (12 max (4 max (55 max 81))))
-- ==> 3 max (6 max (12 max (4 max 81)))
-- ==> 3 max (6 max (12 max 81))
-- ==> 3 max (6 max 81)
-- ==> 3 max 81
-- ==> 81
-- foldr (\x y -> (x+y)/2) 54 [24, 4, 10, 6]
-- ==> 24 (\x y -> (x+y)/2) (foldr (\x y -> (x+y)/2) 54 [4, 10, 6])
-- ==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (foldr (\x y -> (x+y)/2) 54 [10, 6]))
-- ==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (10 (\x y -> (x+y)/2) (foldr (\x y -> (x+y)/2) 54 [6])))
-- ==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (10 (\x y -> (x+y)/2) (6 (\x y -> (x+y)/2) (foldr (\x y -> (x+y)/2) 54 []))))
-- ==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (10 (\x y -> (x+y)/2) (30)
-- ==> 24 (\x y -> (x+y)/2) (4 (\x y -> (x+y)/2) (20)
-- ==> 24 (\x y -> (x+y)/2) (12)
-- ==> 18
-- foldl (\x y -> (x+y)/2) 54 [2, 4, 10, 6]
-- ==> foldl (\x y -> (x+y)/2) (54 (\x y -> (x+y)/2) 2) [4, 10, 6]
-- ==> foldl (\x y -> (x+y)/2) ((54 (\x y -> (x+y)/2) 2) (\x y -> (x+y)/2) 4) [10, 6]
-- ==> foldl (\x y -> (x+y)/2) (((54 (\x y -> (x+y)/2) 2) (\x y -> (x+y)/2) 4) (\x y -> (x+y)/2) 10) [6]
-- ==> foldl (\x y -> (x+y)/2) ((((54 (\x y -> (x+y)/2) 2) (\x y -> (x+y)/2) 4) (\x y -> (x+y)/2) 10) (\x y -> (x+y)/2) 6) []
-- ==> (( 28 (\x y -> (x+y)/2) 4) (\x y -> (x+y)/2) 10) (\x y -> (x+y)/2) 6
-- ==> (16 (\x y -> (x+y)/2) 10) (\x y -> (x+y)/2) 6
-- ==> 13 (\x y -> (x+y)/2) 6
-- ==> 9.5
-- foldl (/) 64 [4, 2, 4]
-- ==> foldl (/) ((/) 64 4) [2, 4]
-- ==> foldl (/) (((/) 64 4) (/) 2) [4]
-- ==> foldl (/) (((64 / 4) /2) /4) []
-- ==> ((16) / 2) / 4
-- ==> 8 / 4
-- ==> 2
-- foldl (\x y -> 2*x + y) 8 [1, 2, 3]
-- ==> foldl (\x y -> 2*x + y) ((\x y -> 2*x + y) 8 1) [2, 3]
-- ==> foldl (\x y -> 2*x + y) (((\x y -> 2*x + y) 8 (\x y -> 2*x + y)1 2)) [3]
-- ==> foldl (\x y -> 2*x + y) (((8 (\x y -> 2*x + y) 1) (\x y -> 2*x + y) 2) (\x y -> 2*x + y) 3) []
-- ==> ((17 (\x y -> 2*x + y) 2) (\x y -> 2*x + y) 3)
-- ==> (36 (\x y -> 2*x + y) 3)
-- ==> 75



zad1 [] = []
zad1 (x:xs) = (zad1 xs) ++ [x]

zad2 arr = last arr : (init (tail arr)) ++ [head arr]

zad3a arr = length [x | x <- arr, even x]

zad3b a = length [x | x <- [1..a], x `mod` 3 == 0]

zad3c a = sum [x | x <- [1..a], x `mod` 3 == 0]

zad4 arr = even (length arr)

zad5a arr = map (^2) arr

zad5b arr = [x^2 | x <- arr]

zad6 a arr = length (filter (==a) arr)

zad7 a num = [a | _ <- [1..num]]

zad8 a
    | a == reverse a = True
    | otherwise = False

zad9 y [] = []
zad9 y (x:xs) = if y == x then xs else x : zad9 y xs

zad10 i [] = []
zad10 i (x:xs)
    | i == 0 = xs
    | otherwise = x : zad10 (i-1) xs

zad11 [] _ = True
zad11 (x:xs) arr2
    | elem x arr2 = zad11 xs arr2
    | otherwise = False

swap (a, b) = (b, a)

zad12 arr = map (swap) arr
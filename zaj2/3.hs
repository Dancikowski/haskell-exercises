zad3a arr = length [x | x <- arr, even x]

zad3b a = length [x | x <- [1..a], x `mod` 3 == 0]

zad3c a = sum [x | x <- [1..a], x `mod` 3 == 0]
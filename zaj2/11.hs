zad11 [] _ = True
zad11 (x:xs) arr2
    | elem x arr2 = zad11 xs arr2
    | otherwise = False
module Computations where
    podzbior [] [] = True
    podzbior _ [] = False
    podzbior [] _ = True
    podzbior (x:xs) y
        | elem x y == True = podzbior xs y
        | otherwise = False

    
    iloczyn _ [] = []
    iloczyn [] _ = []
    iloczyn (x:xs) y
        | elem x y == True = x : iloczyn xs y
        | otherwise = iloczyn xs y

    suma x y = x ++ filter(\n -> not (n `elem` x)) y

    roznica x y = filter(\n -> not (n `elem` y)) x
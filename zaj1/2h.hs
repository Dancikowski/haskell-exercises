zadanie1h a n acc 
    | n == 0 = acc
    | otherwise = zadanie1h a (n-1) (a*acc)
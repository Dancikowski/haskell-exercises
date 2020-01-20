deleteNth i [] = []
deleteNth i (x:xs)
    | i == 0 = xs
    | otherwise = x : deleteNth (i-1) xs
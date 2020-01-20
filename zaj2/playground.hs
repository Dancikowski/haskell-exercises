silnia n = silniaPOM n 1
silniaPOM n x = if n == 0 then x
     else silniaPOM (n-1) (n*x)

fib n = fibPOM n 1 1
fibPOM n f1 f2 = if n == 1 then f1
    else fibPOM (n-1) (f1+f2) f1

mnm [] = error "empty list"
mnm [x] = x
mnm (x:xs) = min x (mnm xs)

removeFst x [] = []
removeFst x (y:ys)
    | x == y = ys
    | otherwise = y : (removeFst x ys)

sort xs = m : (sort (removeFst m xs)) where m = mnm xs
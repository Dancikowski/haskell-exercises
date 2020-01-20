fib a
    | a == 0 = 1
    | a == 1 = 1
    | otherwise =  fib(a - 1) + fib(a -2)

zadanie1i a 
    | a == fib 10 = True
    | otherwise = False
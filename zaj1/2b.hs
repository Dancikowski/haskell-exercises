gcd a b 
   |gcd  b == 0 = a
   |otherwise = gcd b (a `mod` b)

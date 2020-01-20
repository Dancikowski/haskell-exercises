my_lcm :: (Integral a) => a -> a -> a
my_lcm _ 0 =  0
my_lcm 0 _ =  0
my_lcm x y =  (abs (x*y)) `div` gcd x y 
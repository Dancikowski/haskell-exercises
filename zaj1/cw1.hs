import Data.List
zad2a x
    |x > 2 = x^2
    |x > 0 && x <= 2 = x -1
    |x <= 0 = abs x
    |otherwise = x

zad2b a b 
   | zad2b b == 0 = a
   | otherwise = zad2b b (a `mod` b)

zad2c a b
	| b == 0    = a
	| otherwise = zad2c b (a `mod` b)

zad2d a b c
    | the_biggest < sum all_except = True
    | otherwise = False
    where the_biggest = maximum [a, b, c]
          all_except = init (sort[a,b,c])

zad2e r h = 1/3 * pi * r ^ 2 * h

zad2f r h = sqrt(h^2 + r^2)

zad2g a n
    | n == 0 = 1
    | otherwise = a * zad2g a (n - 1)

zad2h a n acc 
    | n == 0 = acc
    | otherwise = zad2h a (n-1) (a*acc)

fib a
    | a == 0 = 1
    | a == 1 = 1
    | otherwise =  fib(a - 1) + fib(a -2)

zad2i a 
    | a == fib 10 = True
    | otherwise = False

zad3a a arr = a:arr

zad3b a arr = head arr :  a : tail arr

zad3c a arr = arr ++ [a]

zad4a arr = head (tail arr)
zad4b arr = head (tail (tail arr))
zad4c arr = last (init arr)
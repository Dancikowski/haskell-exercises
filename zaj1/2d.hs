import Data.List

my_triangle a b c
    | the_biggest < sum all_except = True
    | otherwise = False
    where the_biggest = maximum [a, b, c]
          all_except = init (sort[a,b,c])



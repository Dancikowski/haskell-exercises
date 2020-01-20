my_reverse [] = []
my_reverse (x:xs) = (my_reverse xs) ++ [x]

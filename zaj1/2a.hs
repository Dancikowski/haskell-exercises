mathFun x
    |x > 2 = x^2
    |x > 0 && x <= 2 = x -1
    |x <= 0 = abs x
    |otherwise = x

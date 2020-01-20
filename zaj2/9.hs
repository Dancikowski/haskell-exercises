delete' y [] = []
delete' y (x:xs) = if y == x then xs else x : delete' y xs
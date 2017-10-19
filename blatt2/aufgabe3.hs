doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
	then x
	else x*2

second xs      =  head (tail xs)
swap (x, y)    =  (y, x)
pair x y       =  (x, y)
double x       =  x * 2
palindrome xs  =  reverse xs == xs
twice f x      =  f (f x)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concat'' xs = foldl (++) [] xs 

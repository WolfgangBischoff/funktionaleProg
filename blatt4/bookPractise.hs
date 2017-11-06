--using fold for summing up a list
sum' xs = foldl (+) 0 xs

foldr' f b [] = b
foldr' f b (x:xs) = x `f` foldr f b xs


main = print(sum' [1,2,3,4])

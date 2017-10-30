--rotates list of int
rotate :: Int -> [a] -> [a]
rotate n (x:xs)
 |n==0 = x:xs
 |n==1 = xs ++ x:[]
 |n>1 = rotate (n-1) (rotate 1 (x:xs))

--Rotates every char of a string
encode :: Int -> String -> String
encode n xs
 |n==0 = xs
 |n>0 = map (rot n) xs

--Rotates one char
rot :: Int -> Char -> Char
rot n c
 |n==0 = c
 |n==1 = head (tail init)
 |n>0 = rot (n-1) (rot 1 (c))
 where 
 init = [c..'z'] ++ ['a'..c]

--Returs a list of floats that represent the frequenty of a single char in the input string
freqs :: String -> [Double]
freqs str = ret
 where 
 numbChar str c = sum[1|x<-str, x==c]
 numArr = map (numbChar str) ['a'..'z']
 ret = map (/ len) numArr
 len = fromIntegral(length str)

tableEn = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]


word = "zoskluxxkgrktixevzout"

chisqr engTab a = ret
 where
 tmp = zip engTab a
 chi (es,os)
  |es == 0 = 0.0
  |otherwise = ((os-es)^2)/es
 ret = sum(map chi tmp)

crack inp = ret
 where
 all = replicate 26 inp
 num = [1..26]
 tup = zip num all
 encode' (n, xs) = encode n xs
 possible = map encode' tup
 f = map freqs possible
 calc = map (chisqr tableEn) f
 poscal = zip possible calc
 ret = [x| x<-poscal, snd x == minimum calc]



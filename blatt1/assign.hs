-- Aufgabe 3

{-
N = a 'div' length xs
where
a = 10
xs  = [1,2,3,4,5]

` statt ' (funktion als infix)
liste lässt sich kürzer schreiben
N hat keinen Constructor??
-}

a = 10
xs = [1..5]
nnn = a `div` length xs
--main = print(nnn)
-- 2


-- AUFGABE 4
--main = print(last xs)
-- 5
-- alternatively we could define last a the last added element

-- AUFGABE 5
--main = print(init xs)
--[1,2,3,4]


-- AuFGABE 6
gcds a 0 = a
gcds a b = gcd b (mod a b)

{-
6 4 -> 4 (2)
4 2 -> 2 (0)
2 0 -> 2
-}

main = print(gcds 6 4)
--2



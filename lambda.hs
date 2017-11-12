import Data.List

type Id = String
data Term = Var Id | App Term Term | Abs Id Term
-- Variable | Anwendung funktion parameter | Abstraktion paramter funktion

--Example terms
t = Abs "x"  (Var "x") -- lambda x.x
t2 = App (Var "x") (Var "y") -- x.x
t3 = App (Abs "x"  (Var "x")) (Var "x") -- (lamda x.x) x
t4 = Abs "x" (App (App (Var "x") (Var "y")) (Var "z")) -- \x. ((x y) z)

--Finds all variables of a term
vars (Var a) = a:[]
vars (App t z) = nub (vars(t) ++ vars(z))
vars (Abs q w) = nub (q : vars(w))

fvs (Var a) = a:[]
fvs (App t z) = fvs(t) `union` fvs(z)
-- fvs (App t z) = nub (vars(t) ++ vars(z))
fvs (Abs q w) = fvs(w) \\ [q]


isAscending :: Ord a => [a] -> Bool
isAscending [x] = True
isAscending (x:xs) = x <= head (xs) && isAscending xs
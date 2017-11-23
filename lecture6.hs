
-- Reduce add 2 3
-- add x y
-- lefmost innermost reduction, Parameter??
= @(λmnfx.m f (n f x)) (λfx. f (f(x)))@ (λfx.f (f x)) //m ersetzt
-> (λnfx. @(λfx.f (f (f x))) f@ (n f x)) (λfx.f (f x)) //f ersetzt, fällt weg
-> @(λnfx.(λx.f(f(f x))) (n f x))@ (λfx.f(f x)) //x ersetzt durch (n f x), λ fällt weg
-> @(λnfx.f(f(f(n f x)))) (λfx.f(f x))@ //n ersetzen mit (λfx.f(f x))
-> λfx.f(f(f(@(λfx.f(f x))f@ x))) //f ersetzen durch f, fällt weg
-> λfx.f(f(f(@(λx.f(f x))x @))) //x ersetzen durch x, fällt weg
-> λfx.f(f(f(f(f x))))

-- leftmost outermost reduction, formeln auflösen
= @(λmnfx.m f (n f x)) (λfx. f (f(x)))@ (λfx.f (f x)) //m ersetzt
-> @(λnfx. (λfx.f (f (f x))) f (n f x)) (λfx.f (f x))@ //n ersetzen mit (λfx.f (f x))
-> λfx. @(λfx.f (f (f x))) f@ ((λfx.f (f x) f x) //f durch f ersetzen, fällt weg
-> λfx. @(λx.f (f (f x))) ((λfx.f (f x) f x)@ //x durch ((λfx.f (f x) f x) ersetzen
-> λfx. (f (f (f @(λfx.f (f x)) f@ x))) //f fällt weg
-> λfx. (f (f (f @(λx.f (f x)) x@))) //x fällt weg
-> λfx. (f (f (f (f (f x)))))

data BTree a = Empty | Node a (BTree a) (BTree a)
 deriving (Eq, Show, Read)
newtype Set a = Set { rep :: BTree a }

set = Node 2 (Node 1 Empty Empty) (Node 4 (Node 3 Empty Empty)(Node 5 Empty Empty)) -- same
set2 = Node 2 (Node 1 Empty Empty) (Node 4 (Node 3 Empty Empty)(Node 5 Empty Empty)) -- same
set3 = Node 2 (Node 1 Empty Empty) (Node 4 (Node 3 Empty Empty)(Node 15 Empty Empty)) -- not the same

-- inserts an element into a set
insertTree :: Ord a => a -> BTree a -> BTree a
insertTree x Empty        = Node x Empty Empty
insertTree x (Node y l r) =
 case compare x y of
 EQ -> Node y l r
 LT -> Node y (insertTree x l) r
 GT -> Node y l (insertTree x r)

-- returns a list instead of a set
getList Empty = []
getList (Node a l r) = getList l ++ [a] ++ getList r

-- Compares if two sets are equal
-- equals :: Ord a => Set a -> Set a -> Bool
equals a b = (getList a) == (getList b)


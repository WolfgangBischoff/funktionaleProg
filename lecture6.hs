
-- Reduce add 2 3
add x y = x + y



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


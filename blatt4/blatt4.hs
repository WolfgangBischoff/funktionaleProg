-- checking whether the given tree is a search tree

data BTree a = Empty | Node a (BTree a) (BTree a)
 deriving (Eq, Show, Read)

tree = Node 2 (Node 1 Empty Empty) (Node 4 (Node 3 Empty Empty)(Node 5 Empty Empty))
treeFail = Node 5 (Node 1 Empty Empty) (Node 4 (Node 3 Empty Empty)(Node 1 Empty Empty))

isSorted :: Ord a => BTree a -> Bool
isSorted a = isAscending list
 where list = getList a

getList Empty = []
getList (Node a l r) = getList l ++ [a] ++ getList r

isAscending :: Ord a => [a] -> Bool
isAscending [x] = True
isAscending (x:xs) = x <= head (xs) && isAscending xs

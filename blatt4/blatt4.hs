-- checking whether the given tree is a search tree
import Data.Tree

data BTree a = Empty | Node a (BTree a) (BTree a)
 deriving (Eq, Show, Read)
--isSorted :: Ord a => BTree a -> Bool




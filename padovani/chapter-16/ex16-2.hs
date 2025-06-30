-----------------------------------------------------------
-- Exercise 16.2

{- sortList first builds a binary search tree from the 
   input list by inserting each of its elements into the 
   tree. After that, it traverses the tree in-order and
   returns this traversal as a list.
   The resulting list is sorted and has no duplicate
   elements.
-}

data Bst a = Empty | Node a (Bst a) (Bst a)
  deriving (Show, Eq)

-- inserts the elements of an input list into a bstree
lstToBst :: Ord a => [a] -> Bst a
lstToBst = foldr insert Empty

-- inserts an input element into a given bstree
-- assumes the list has no duplicate elements
insert :: (Ord a) => a -> Bst a -> Bst a
insert x Empty = Node x Empty Empty
insert x (Node y l r)
  | x < y = Node y (insert x l) r
  | otherwise = Node y l (insert x r)

-- traverses an input bstree in-order and puts
-- the traversal in a list
inOrder :: Bst a -> [a]
inOrder Empty = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r

-- sorts an input list that has no duplicate 
-- elements using a bstree 
sortLst :: (Ord a) => [a] -> [a]
sortLst = inOrder . lstToBst
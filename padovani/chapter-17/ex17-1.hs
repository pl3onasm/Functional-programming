-----------------------------------------------------------
-- Exercise 17.1

module Sets (
  Set,     -- type is exported, but not its representation
  emptySet,
  isEmpty,
  mkSingleton,
  isMember,
  add,
  toSet,
  setUnion,
  setIntersect,
  setDiff,
) where

data Bst a = Empty | Node a (Bst a) (Bst a)
  deriving (Eq)

-- we define Set as an alias for a Bst
type Set a = Bst a

-- defines an instance of Show for sets
instance Show a => Show (Set a) where
  show s = "{" ++ shw (toList s) ++ "}"
    where
      shw []       = ""
      shw [x]      = show x
      shw (x : xs) = show x ++ ", " ++ shw xs

-- represents the empty set
emptySet :: Set a
emptySet = Empty

-- checks whether a set is empty
isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- creates a singleton, given an input element
mkSingleton :: a -> Set a
mkSingleton x = Node x Empty Empty

-- checks whehter an element belongs to a given set
isMember :: Ord a => a -> Set a -> Bool
isMember _ Empty = False
isMember x (Node y l r)
  | x == y    = True
  | x < y     = isMember x l 
  | otherwise = isMember x r

-- adds an element to a set
add :: Ord a => a -> Set a -> Set a
add x Empty = mkSingleton x
add x (Node y l r)
  | x == y    = Node y l r
  | x < y     = Node y (add x l) r 
  | otherwise = Node y l (add x r)

-- converts a set of values into a list of values
toList :: Set a -> [a]
toList Empty = []
toList (Node x l r) = toList l ++ [x] ++ toList r

-- converts a list of values into a set of values
toSet :: Ord a => [a] -> Set a
toSet = foldr add emptySet

-- computes the union of two input sets
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion xs ys = foldr add xs (toList ys) 

-- alternative implementation for setUnion
setUnion' :: Ord a => Set a -> Set a -> Set a
setUnion' xs ys = toSet (toList xs ++ toList ys)

-- computes the intersection of two input sets
setIntersect :: Ord a => Set a -> Set a -> Set a
setIntersect xs ys = foldr include emptySet (toList xs)
  where
    include = (\x acc -> if isMember x ys 
                         then add x acc 
                         else acc)

-- alternative implementation for setIntersect
setIntersect' :: Ord a => Set a -> Set a -> Set a
setIntersect' xs ys = toSet [x | x <- toList xs, 
                                 isMember x ys]

-- computes the difference of two input sets 
setDiff :: Ord a => Set a -> Set a -> Set a
setDiff xs ys = foldr exclude emptySet (toList xs)
  where
    exclude = (\x acc -> if isMember x ys
                         then acc
                         else add x acc)

-- alternative implementation for setDiff
setDiff' :: Ord a => Set a -> Set a -> Set a
setDiff' xs ys = toSet [x | x <- toList xs, 
                            not (isMember x ys)]

-----------------------------------------------------------

{- Example usage in GHCi:
  
  xs = toSet [1,3,1,5,4,3,1,7,8,0]
  xs
    {0, 1, 3, 4, 5, 7, 8}
  ys = toSet [2,0,4,1,0,6,9]
  ys 
    {0, 1, 2, 4, 6, 9}
  setUnion xs ys
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
  setIntersect xs ys
    {0, 1, 4}
  setDiff xs ys
    {3, 5, 7, 8}
  setDiff ys xs
    {2, 6, 9}
-}
-----------------------------------------------------------
-- Exercise 8.2

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

-- the function that was given to check
-- if an element occurs in the search tree
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) 
  | x == y = True
  | x < y = occurs x l
  | otherwise = occurs x r

-- modified function using compare
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = case x `compare` y of
  EQ -> True
  _  -> False
occurs' x (Node l y r) = case x `compare` y of
  EQ -> True
  LT -> occurs' x l
  GT -> occurs' x r

-----------------------------------------------------------

{-
  The new function occurs' is more efficient than the
  original occurs function because it uses the compare
  function, which performs a single comparison and returns 
  an Ordering value: LT, EQ, or GT.

  In the original occurs function, up to three comparisons 
  may be performed at each node to determine whether the 
  target element x is equal to, less than, or greater than
  the current node's value y:

  First, it checks if x == y.
  If not, it checks if x < y.
  If both checks fail, it assumes x > y.

  These are logically independent comparisons and may 
  involve multiple evaluations, depending on the type and 
  how equality and ordering are implemented.

  In contrast, occurs' uses a single call to compare x y, 
  which directly evaluates the ordering relationship 
  between the two values and returns the result as one of 
  three distinct outcomes. The function then pattern 
  matches on the result and proceeds accordingly, without
  performing any redundant comparisons.

  As a result, for each node visited, occurs' performs
  exactly one comparison operation, making it more 
  efficient, especially for types where comparisons are 
  relatively expensive.

-}
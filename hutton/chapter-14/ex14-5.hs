-----------------------------------------------------------
-- Exercise 14.5

-- filterF :: (Foldable t) => (a -> Bool) -> t a -> [a]
filterF :: (Foldable t) => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then pure x else mempty)

-----------------------------------------------------------
{-
  This is a generic implementation of the standard filter 
  function that works on any type that is an instance of 
  the Foldable type class.

  The function takes a predicate p :: a -> Bool and a 
  foldable structure t a, and returns a list of all 
  elements in the structure that satisfy the predicate.

  It is defined using foldMap, which maps each element to a 
  monoidal value and combines the results. In this case, we 
  map each element x to [x] (using pure) if it satisfies 
  the predicate, or to [] (using mempty) otherwise.

  Since lists form a monoid under concatenation (++),
  foldMap will concatenate all the singleton lists for
  elements that satisfy the predicate. If no elements
  satisfy the predicate, it will return an empty list.

  Examples:

  ghci> :l ex14-5.hs
  ghci> filterF even [1..10]
  [2,4,6,8,10]
  ghci> filterF (< 5) (Just 7)
  []
  ghci> filterF (< 5) (Just 3)
  [3]
  ghci> t = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
  > filterF (> 1) t
  [2,3]
 
-}
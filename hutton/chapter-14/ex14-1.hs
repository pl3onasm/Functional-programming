import Data.Monoid

-----------------------------------------------------------
-- Exercise 14.1 (with newtype wrapper + Semigroup)

newtype Pair a b = P (a, b)
  deriving (Show, Eq)

instance (Monoid a, Monoid b) => Semigroup (Pair a b) where
  -- (<>) :: Pair a b -> Pair a b -> Pair a b
  P (x1, y1) <> P (x2, y2) = P (x1 <> x2, y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  -- mempty :: Pair a b
  mempty = P (mempty, mempty)

  -- mappend defaults to (<>) 
  mappend = (<>)

-----------------------------------------------------------

{-
  Since GHC 8.4 every Monoid must also be a Semigroup.
  This is why we first define a Semigroup instance
  for Pair a b before defining the Monoid instance.
  We also had to use a newtype wrapper because GHC 
  already has a Monoid instance for tuples (a, b) 
  where both a and b are Monoids.

  The instance declaration defines a Monoid for pairs
  (a, b) where both components are themselves Monoids. 
  The mempty is defined as a pair of the mempty
  values of the respective types, and mappend combines
  the two pairs by applying mappend to each component.
  This allows for the combination of pairs in a way that
  respects the Monoid laws.

  The monoid laws are satisfied because:
  1. Identity: mempty acts as a neutral element for both
     components. For any P (x, y), we have:
       P (x, y) <> mempty == P (x, y)
       mempty <> P (x, y) == P (x, y)

  2. Associativity: the operation (<>) is associative
     component-wise:
       (P (x1, y1) <> P (x2, y2)) <> P (x3, y3)
       == P (x1 <> x2 <> x3, y1 <> y2 <> y3)
       == P (x1, y1) <> (P (x2, y2) <> P (x3, y3))

  3. Closure: since both a and b are Monoids, the result
     of combining two values is still a valid value of 
     type Pair a b.

  Examples:
    
  ghci> P ("Hi, ", [1]) <> P ("there!", [2,3])
  P ("Hi, there!", [1,2,3])

  ghci> P (Sum 3, Product 4) <> P (Sum 7, Product 5)
  P (Sum {getSum = 10}, Product {getProduct = 20})

  ghci> mempty :: Pair String [Int]
  P ("", [])

  ghci> mconcat [P ("Hello, ", [1]), P ("world!", [2, 3])]
  P ("Hello, world!", [1, 2, 3])
  
-}

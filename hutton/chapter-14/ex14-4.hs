import Data.Foldable
import Data.Traversable

-----------------------------------------------------------
-- Exercise 14.4

data Tree a = Leaf 
            | Node (Tree a) a (Tree a)
            deriving Show

instance Foldable Tree where

  -- fold :: monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l x r) = fold l <> x <> fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = 
    foldMap f l <> f x <> foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ z Leaf = z
  foldl f z (Node l x r) = foldl f (foldl f (f z x) l) r

instance Traversable Tree where

  -- traverse :: Applicative f =>
  --             (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Node l x r) =
    Node <$> traverse f l <*> f x <*> traverse f r

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-----------------------------------------------------------

{-
  The Tree type is a simple binary tree with values stored
  at internal nodes. We derive Functor, Foldable, and
  Traversable instances to make the tree structure behave
  like a container that supports mapping, folding, and
  effectful traversals.

  The Foldable instance allows us to reduce a tree to a 
  single summary value using monoidal combination or 
  left/right folds.

    - fold reduces the tree by inserting mempty at the 
      leaves and combining node values in-order

    - foldMap maps each element to a monoid and combines 
      the results using (<>)

    - foldr performs a right-associative fold

    - foldl performs a left-associative fold


  The Traversable instance allows us to apply an effectful
  function to each element of the tree, preserving 
  structure and evaluating in in-order sequence.

  The Functor instance maps a pure function over all values
  in the tree, recursively applying it to every node.

  Examples:
    ghci> t = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

    ghci> fold t :: Sum Int
    Sum {getSum = 6}

    ghci> foldMap show t
    "123"

    ghci> foldr (:) [] t
    [1,2,3]

    ghci> foldl (flip (:)) [] t
    [3,2,1]

    ghci> fmap (*10) t
    Node (Node Leaf 10 Leaf) 20 (Node Leaf 30 Leaf)

    ghci> traverse (\x -> [x, x+1]) t
    [Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf),
     Node (Node Leaf 1 Leaf) 2 (Node Leaf 4 Leaf),
     Node (Node Leaf 1 Leaf) 3 (Node Leaf 3 Leaf),
     Node (Node Leaf 1 Leaf) 3 (Node Leaf 4 Leaf),
     Node (Node Leaf 2 Leaf) 2 (Node Leaf 3 Leaf),
     Node (Node Leaf 2 Leaf) 2 (Node Leaf 4 Leaf),
     Node (Node Leaf 2 Leaf) 3 (Node Leaf 3 Leaf),
     Node (Node Leaf 2 Leaf) 3 (Node Leaf 4 Leaf)]

-}
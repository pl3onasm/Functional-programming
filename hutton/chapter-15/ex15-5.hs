-----------------------------------------------------------
-- Exercise 15.5

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show)


repeatT :: a -> Tree a
repeatT x = Node (repeatT x) x (repeatT x)

takeT :: Int -> Tree a -> Tree a
takeT 0 _    = Leaf
takeT _ Leaf = Leaf
takeT n (Node r x l) = 
  Node (takeT (n - 1) l) x (takeT (n - 1) r)

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT

-----------------------------------------------------------

{- 

  - repeatT creates an infinite perfectly balanced binary
    tree where each node contains the same value x.
    It is defined recursively and relies on Haskell's lazy 
    evaluation to avoid looping forever.

  - takeT returns a finite tree of depth n by truncating
    the input tree. A depth of 0 returns Leaf, and deeper
    trees are recursively truncated.

  - replicateT combines both functions to build a finite
    perfectly balanced binary tree of depth n, where each 
    node contains the value x. It first creates an infinite 
    tree with repeatT and then truncates it to the desired 
    depth using takeT.

  Example:
    ghci> replicateT 2 5
    Node (Node Leaf 5 Leaf) 5 (Node Leaf 5 Leaf)

-}
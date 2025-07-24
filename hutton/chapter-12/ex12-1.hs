-----------------------------------------------------------
-- Exercise 12.1

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-----------------------------------------------------------

{-
  The idea of a Functor is to enrich a type with a mapping
  function that lifts a pure function into the context of 
  that type, so that it can be applied to all the values 
  across the structure of that type.

  For the Tree type, fmap applies the function g to each 
  value in the tree, while preserving the structure of the 
  tree. That is, if we have a function g :: a -> b that
  transforms values of type a into values of type b, and
  we apply fmap g to a Tree with values of type a, we will
  get a new Tree with values of type b, where each value
  in the original tree has been transformed by g.
  
  As usual, the type signature of fmap guides us through
  the implementation process.

  The first case handles the empty tree (Leaf), where there
  are no values to transform. Since Leaf contains no 
  values, it can be returned as Tree b: the type variable 
  changes, but the structure remains valid.
  
  The second case handles a non-empty tree (Node): here, 
  we apply g to the current node's value (x), and 
  recursively fmap g over the left and right subtrees 
  (l and r). This preserves the original shape of the 
  tree while transforming the values.

  The types also check out for the recursive case:
  - The left subtree l is of type Tree a, and after 
    applying fmap, it becomes Tree b.
  - The value x is of type a, and after applying g, it
    becomes type b.
  - The right subtree r is also of type Tree a, and after
    applying fmap, it becomes Tree b.
  - Putting these together, we return a single Tree b
    by packing them up into a Node.
-}
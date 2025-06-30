----------------------------------------------------------------
-- Exercise 16.5

{- Checks whether a binary search tree satisfies the BST 
   property, i.e., whether the values in the tree are
   ordered in a way that for each node, all values in the
   left subtree are less than the node's value, and all
   values in the right subtree are greater than the node's
   value. 

   This is done by passing down a range of valid bounds
   for each subtree, which is updated as we traverse
   the tree. At each node, we check whether the value
   of the node is within the valid bounds. If it is,
   we recursively check the left and right subtrees with
   updated bounds. If any node violates the BST property,
   the function returns False.

   We start with an initial range of (-∞, +∞) for the root 
   node, and as we traverse the tree, we update the bounds 
   based on the value x of the current node. The left 
   subtree's upper bound is the current node's
   value minus one (x - 1), and the right subtree's lower 
   bound is the current node's value plus one (x + 1).

   The exercise says we need to use ranges as implemented  
   in exercise 15.7, where a range is defined as a pair of
   Maybe Int values, representing the lower and upper bounds.
   This means that the values of the BST can be integer
   values only.
-}

-- maybe data type for Range
data Range = Range (Maybe Int) (Maybe Int) | EmptyRange
  deriving (Eq, Show)

-- data type for binary search tree
data Bst a = Node a (Bst a) (Bst a) | EmptyBst
  deriving (Eq, Show)

-- Checks whether a binary search tree satisfies 
-- the BST property
isBst :: Bst Int -> Bool
isBst = check (Range Nothing Nothing)
  where
    check :: Range -> Bst Int -> Bool
    check _ EmptyBst = True
    check (Range lo hi) (Node x l r) =
      inRange x lo hi &&
      check (Range lo (Just (x - 1))) l &&  -- does not allow 
      check (Range (Just (x + 1)) hi) r     -- duplicate values 

    inRange :: Int -> Maybe Int -> Maybe Int -> Bool
    inRange x Nothing Nothing     = True             -- [-∞,+∞]
    inRange x (Just a) Nothing    = a <= x           -- [a, +∞]
    inRange x Nothing (Just b)    = x <= b           -- [-∞, b]
    inRange x (Just a) (Just b)   = a <= x && x <= b -- [a,  b]

----------------------------------------------------------------

{- We can easily update the `isBst` function to work with
   any type of value, not just Int.
   This requires us to define a new `Range` data type that
   can work with any type `a` that is an instance of `Ord`,
   and to update the verification function accordingly.
-}

data Ran a = Ran (Maybe a) (Maybe a) | EmptyRan
  deriving (Eq, Show)

-- Checks whether a binary search tree satisfies
-- the BST property
isBST :: Ord a => Bst a -> Bool
isBST = check (Ran Nothing Nothing)
  where
    check :: Ord a => Ran a -> Bst a -> Bool
    check _ EmptyBst = True
    check (Ran lo hi) (Node x l r) =
      inRange x lo hi &&
      check (Ran lo (Just x)) l &&   -- allows duplicate values  
      check (Ran (Just x) hi) r      

    inRange :: Ord a => a -> Maybe a -> Maybe a -> Bool
    inRange x Nothing Nothing   = True
    inRange x (Just a) Nothing  = a <= x
    inRange x Nothing (Just b)  = x <= b
    inRange x (Just a) (Just b) = a <= x && x <= b

----------------------------------------------------------------

{- Example usage in GHCi:

        10                        20
       /  \                      /  \
      5    20                  15    30
     / \     \                  \    / \  
    1   8     25                 2  18 32

       tree1                     tree2

:{
tree1 = 
  Node 10 
  (Node 5 (Node 1 EmptyBst EmptyBst) 
  (Node 8 EmptyBst EmptyBst)) 
  (Node 20 EmptyBst (Node 25 EmptyBst EmptyBst))
:}

:{
tree2 = 
  Node 20 
  (Node 15 EmptyBst (Node 2 EmptyBst EmptyBst))
  (Node 30 (Node 18 EmptyBst EmptyBst)
  (Node 32 EmptyBst EmptyBst))
:}

isBst tree1
  True
isBst tree2
  False
  
-}
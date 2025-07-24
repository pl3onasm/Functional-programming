-----------------------------------------------------------
-- Exercise 8.3

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

-- computes the number of leaves in the tree
numLeaves :: Tree a -> Int 
numLeaves (Leaf _) = 1
numLeaves (Node l r) = numLeaves l + numLeaves r

-- checks if the tree is balanced, i.e., if the difference
-- in the number of leaves in the left and right subtrees
-- is at most 1. Time complexity: O(n^2)
-- where n is the number of nodes in the tree.
-- This is because numLeaves is called for each node, so
-- in the worst case of an unbalanced tree (a linked list),
-- numLeaves is called on subtrees of increasing size
-- leading to a quadratic number of calls.
isBalanced :: Tree a -> Bool
isBalanced (Leaf _)   = True
isBalanced (Node l r) =
  abs (numLeaves l - numLeaves r) <= 1
  && isBalanced l 
  && isBalanced r

-- A much more efficient version of isBalanced
-- that computes the number of leaves and checks balance
-- in a *single* traversal of the tree. 
-- Time complexity: O(n) where n is the number of nodes
isBalanced' :: Tree a -> Bool
isBalanced' t = snd (check t)
  where
    check :: Tree a -> (Int, Bool)
    check (Leaf _) = (1, True)
    check (Node l r) =
      let (nl, bl) = check l
          (nr, br) = check r
          balanced = abs (nl - nr) <= 1 && bl && br
      in (nl + nr, balanced)

-----------------------------------------------------------

{-
  Examples:
             t1                 t2

            Node               Node
            /  \               /  \
           5  Node          Node  Node
              /  \          /  \  /  \
            Node  6        1   2  3  Node
            /  \                     /  \
           1    2                   4    5       

  ghci> :l ex8-3
  ghci> :{
  ghci| t1 = Node (Leaf 5)
  ghci|      (Node (Node (Leaf 1) (Leaf 2)) (Leaf 6))
  ghci| :}
  ghci> numLeaves t1
  4 
  ghci> isBalanced t1
  False
  ghci> isBalanced' t1
  False
  ghci> :{
  ghci| t2 = Node (Node (Leaf 1) (Leaf 2))
  ghci|      (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))
  ghci| :}
  ghci> numLeaves t2
  5
  ghci> isBalanced t2
  True
  ghci> isBalanced' t2
  True

-}
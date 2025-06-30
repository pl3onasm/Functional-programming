-----------------------------------------------------------
-- Exercise 16.3

data Bst a = Empty | Node a (Bst a) (Bst a)
  deriving (Show, Eq)

-- given a label and a tree, the function returns a
-- path in the form of a list of labels, going from the 
-- root of the tree to the input label
-- Returns the empty tree if the input label is not found
getPath :: (Ord a) => a -> Bst a -> [a]
getPath _ Empty = []
getPath x (Node y l r)
  | x == y    = [y]
  | x < y     = case getPath x l of
                  [] -> []
                  ys -> y : ys
  | otherwise = case getPath x r of
                  [] -> []
                  ys -> y : ys

-----------------------------------------------------------

{- Example tree:

          5
         / \
        3   8
       / \
      2   4

Test in GHCi: 

:{
tree = Node 5
  (Node 3
  (Node 2 EmptyBst EmptyBst)
  (Node 4 EmptyBst EmptyBst))
  (Node 8
  EmptyBst
  (Node 9 EmptyBst EmptyBst))
:}

  getPath 4 tree
    [5,3,4]
  getPath 8 tree
    [5,8]
  getPath 1 tree
    []
-}